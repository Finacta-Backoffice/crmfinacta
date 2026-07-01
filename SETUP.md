# Finacta Leads — Checklist de setup na conta oficial

## Arquivos (estes 4 vão pro GitHub, na raiz)

    index.html        -> formulário conversacional
    _worker.js        -> código que grava no Notion
    wrangler.jsonc    -> configuração do Cloudflare Worker
    .assetsignore     -> impede o _worker.js de ser publicado como site

## Antes de subir

1. Abra o index.html e troque o `whatsappNumber` no bloco CONFIG pelo
   WhatsApp real da Amanda (só dígitos, ex.: 5531999990000).

## No Notion (conta da empresa)

1. Crie uma integração em notion.so/my-integrations. Copie o token (ntn_...).
2. Crie um banco de dados (database, não tabela simples) com estas colunas:

       Nome               -> Title
       WhatsApp           -> Phone
       Email              -> Email
       Como conheceu      -> Text
       Quem indicou       -> Text
       Objetivo           -> Text
       Maior dificuldade  -> Text
       Faixa de Renda     -> Text
       Já planejou antes  -> Text
       Status             -> Select
       Origem             -> Select
       Data de Entrada    -> Date

   ATENÇÃO: os nomes devem bater EXATAMENTE, incluindo maiúsculas e acentos.
   "Faixa de Renda" com R maiúsculo. "Data de Entrada" com E maiúsculo.

3. Na coluna Status, crie: Novo contato, Em conversa, Proposta apresentada,
   Cliente, Não fechou.

4. Conecte a integração ao banco: três pontinhos > Connections > sua integração.

5. Copie o ID do banco (32 caracteres na URL, entre a última barra e o ?).

## No Cloudflare

1. Crie um projeto Workers & Pages conectado ao GitHub.
2. Em Settings > Variables and Secrets, adicione:
   - NOTION_TOKEN (tipo Secret) = seu token ntn_...
   - NOTION_DATABASE_ID (tipo Plaintext) = os 32 caracteres
3. Dê Deploy.

## Testar

1. Abra seusite.workers.dev/api/test — deve mostrar "ACESSO OK" e as colunas.
2. Abra seusite.workers.dev/api/list — deve mostrar o banco na lista.
3. Preencha o formulário até o fim e confira se a linha apareceu no banco.
4. Preencha só até o WhatsApp e pare — deve aparecer como "Parcial".

## Depois que tudo funcionar

- Apague os blocos /api/test e /api/list do _worker.js (são só diagnóstico).
- Troque a visualização do banco pra Board agrupado por Status.
