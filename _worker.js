// _worker.js
// Worker único do Cloudflare. Fica na RAIZ, junto do index.html.
// Serve o site e grava o lead no Notion na rota /api/lead.
//
// Captura progressiva: assim que a pessoa preenche o WhatsApp, cria a linha no
// Notion como "Parcial". A cada resposta seguinte, atualiza a mesma linha. Se ela
// terminar, vira "Novo contato". Se parar no meio, o lead fica salvo como Parcial.
//
// Variáveis de ambiente (Settings > Variables and Secrets no Cloudflare):
//   NOTION_TOKEN         -> segredo da integração  (marque como Secret)
//   NOTION_DATABASE_ID   -> os 32 caracteres do banco
//   NOTION_VERSION       -> opcional, padrão 2022-06-28

const STATUS_PARCIAL  = "Parcial";
const STATUS_COMPLETO = "Novo contato";
const ORIGEM          = "Formulário (Bio Instagram)";

// Mapeia cada resposta do formulário para uma propriedade do Notion.
// OS NOMES DEVEM BATER EXATAMENTE com as colunas do banco, incluindo maiúsculas.
// As cinco de resposta são rich_text (Text) porque o Notion não aceita vírgula
// em opção de Select.
const FIELD_MAP = {
  nome:          { name: "Nome",              type: "title" },
  whatsapp:      { name: "WhatsApp",          type: "phone_number" },
  email:         { name: "Email",             type: "email" },
  como_conheceu: { name: "Como conheceu",     type: "rich_text" },
  quem_indicou:  { name: "Quem indicou",      type: "rich_text" },
  objetivo:      { name: "Objetivo",          type: "rich_text" },
  dificuldade:   { name: "Maior dificuldade", type: "rich_text" },
  faixa_renda:   { name: "Faixa de Renda",    type: "rich_text" },
  ja_planejou:   { name: "Já planejou antes", type: "rich_text" },
};

function buildProp(type, value) {
  value = (value || "").toString().trim();
  if (!value) return null;
  switch (type) {
    case "title":        return { title: [{ text: { content: value } }] };
    case "rich_text":    return { rich_text: [{ text: { content: value } }] };
    case "select":       return { select: { name: value } };
    case "email":        return { email: value };
    case "phone_number": return { phone_number: value };
    default:             return { rich_text: [{ text: { content: value } }] };
  }
}

function buildProperties(answers, opts) {
  opts = opts || {};
  const props = {};
  for (const [id, cfg] of Object.entries(FIELD_MAP)) {
    const p = buildProp(cfg.type, answers[id]);
    if (p) props[cfg.name] = p;
  }
  props["Origem"] = buildProp("select", ORIGEM);
  if (opts.status) props["Status"] = buildProp("select", opts.status);
  props["Data de Entrada"] = { date: { start: new Date().toISOString().slice(0, 10) } };
  return props;
}

function notionHeaders(env) {
  return {
    Authorization: `Bearer ${env.NOTION_TOKEN}`,
    "Notion-Version": env.NOTION_VERSION || "2022-06-28",
    "Content-Type": "application/json",
  };
}

async function createLead(answers, env) {
  try {
    const resp = await fetch("https://api.notion.com/v1/pages", {
      method: "POST",
      headers: notionHeaders(env),
      body: JSON.stringify({
        parent: { database_id: env.NOTION_DATABASE_ID },
        properties: buildProperties(answers, { status: STATUS_PARCIAL }),
      }),
    });
    const text = await resp.text();
    if (resp.ok) {
      const data = JSON.parse(text);
      return Response.json({ ok: true, pageId: data.id });
    }
    console.error("Notion create error:", resp.status, text);
    return Response.json({ ok: false, error: resp.status, detail: text });
  } catch (e) {
    console.error("Notion create failed:", e);
    return Response.json({ ok: false, error: "request failed", detail: e.message });
  }
}

async function updateLead(pageId, answers, complete, env) {
  if (!pageId) return Response.json({ ok: false, error: "no pageId" });
  try {
    const props = buildProperties(answers, complete ? { status: STATUS_COMPLETO } : {});
    const resp = await fetch(`https://api.notion.com/v1/pages/${pageId}`, {
      method: "PATCH",
      headers: notionHeaders(env),
      body: JSON.stringify({ properties: props }),
    });
    const text = await resp.text();
    if (resp.ok) return Response.json({ ok: true });
    console.error("Notion update error:", resp.status, text);
    return Response.json({ ok: false, error: resp.status, detail: text });
  } catch (e) {
    console.error("Notion update failed:", e);
    return Response.json({ ok: false, error: "request failed", detail: e.message });
  }
}

export default {
  async fetch(request, env) {
    const url = new URL(request.url);

    // ROTA DE DIAGNÓSTICO — acesse /api/test no navegador pra ver se o token e o banco funcionam.
    // Pode apagar esse bloco depois que tudo estiver rodando.
    if (url.pathname === "/api/test") {
      const token = env.NOTION_TOKEN || "UNDEFINED";
      const dbId = env.NOTION_DATABASE_ID || "UNDEFINED";
      const tokenPreview = token === "UNDEFINED" ? "UNDEFINED" : token.slice(0, 8) + "...";
      const h = { Authorization: "Bearer " + token, "Notion-Version": env.NOTION_VERSION || "2022-06-28" };
      try {
        const r = await fetch("https://api.notion.com/v1/databases/" + dbId, { headers: h });
        const d = await r.json();
        if (r.ok) {
          const cols = Object.entries(d.properties).map(([k,v]) => k + " (" + v.type + ")").join(", ");
          return Response.json({ status: "ACESSO OK", banco: d.title?.[0]?.plain_text, colunas: cols, tokenPreview, dbId });
        }
        return Response.json({ status: "ERRO", code: d.status, message: d.message, tokenPreview, dbId });
      } catch (e) { return Response.json({ status: "FALHA", error: e.message, tokenPreview, dbId }); }
    }

    // LISTA BANCOS — acesse /api/list pra ver quais bancos o token enxerga.
    // Pode apagar esse bloco depois que tudo estiver rodando.
    if (url.pathname === "/api/list") {
      const h = { Authorization: "Bearer " + (env.NOTION_TOKEN || ""), "Notion-Version": env.NOTION_VERSION || "2022-06-28", "Content-Type": "application/json" };
      try {
        const r = await fetch("https://api.notion.com/v1/search", {
          method: "POST", headers: h,
          body: JSON.stringify({ filter: { property: "object", value: "database" }, page_size: 20 })
        });
        const d = await r.json();
        if (r.ok) {
          const dbs = (d.results || []).map(db => ({ id: db.id, nome: db.title?.[0]?.plain_text || "(sem nome)" }));
          return Response.json({ status: "OK", bancos_visiveis: dbs });
        }
        return Response.json({ status: "ERRO", code: d.status, message: d.message });
      } catch (e) { return Response.json({ status: "FALHA", error: e.message }); }
    }

    // ROTA PRINCIPAL — recebe os leads do formulário
    if (url.pathname === "/api/lead" && request.method === "POST") {
      let body = {};
      try { body = await request.json(); } catch (_) {}

      if (!env.NOTION_TOKEN || !env.NOTION_DATABASE_ID) {
        return Response.json({ ok: true, mode: "dry-run", pageId: "dry-run" });
      }

      const action = body.action || "create";
      if (action === "update") {
        return updateLead(body.pageId, body.answers || {}, body.complete, env);
      }
      return createLead(body.answers || body, env);
    }

    // Qualquer outra rota: serve os arquivos estáticos (o index.html).
    return env.ASSETS.fetch(request);
  },
};
