// Navigate via CDP without enabling Runtime events; read the <pre> text after a fixed wait.
const [url, waitSec] = process.argv.slice(2);
const port = 9334;
const profile = '/tmp/lucuma-bench-chrome-profile';
const { spawn } = await import('node:child_process');
const chrome = spawn('/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  ['--headless=new', '--disable-gpu', `--remote-debugging-port=${port}`, `--user-data-dir=${profile}`, 'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
let targets;
for (let i = 0; i < 50; i++) { try { targets = await (await fetch(`http://127.0.0.1:${port}/json`)).json(); break; } catch { await sleep(200); } }
const page = targets.find(t => t.type === 'page');
let id = 0; const pending = new Map();
const onmsg = ev => { const m = JSON.parse(ev.data); if (m.id && pending.has(m.id)) { pending.get(m.id)(m.result); pending.delete(m.id); } };
const connect = async () => { const w = new WebSocket(page.webSocketDebuggerUrl); w.onmessage = onmsg; await new Promise(r => w.onopen = r); return w; };
const ws = await connect();
await new Promise(res => { pending.set(++id, res); ws.send(JSON.stringify({ id, method: 'Page.navigate', params: { url } })); });
ws.close();
await sleep(Number(waitSec) * 1000);
const ws2 = await connect();
const r = await new Promise(res => { pending.set(++id, res); ws2.send(JSON.stringify({ id, method: 'Runtime.evaluate', params: { expression: "document.getElementById('out').textContent", returnByValue: true } })); });
console.log(r?.result?.value ?? JSON.stringify(r));
ws2.close(); chrome.kill();
