// Run with Node and Playwright; see scripts/README.md.
import { pathToFileURL } from 'node:url';

export async function checkWebR(page, url) {
  await page.goto(url);
  // Fail immediately on the original startup error, rather than timing out.
  await page.waitForFunction(() => {
    if (document.querySelector('.observablehq--error')) return true;
    return document.querySelectorAll('.cm-editor').length > 0 &&
      !document.querySelector('#exercise-loading-indicator');
  }, undefined, { timeout: 180_000 });
  const errors = await page.locator('.observablehq--error').allTextContents();
  if (errors.length) throw new Error(`webR package initialization failed: ${errors[0]}`);

  // This chapter's first visible cell depends on its hidden setup and data files.
  const cell = page.locator('#webr-2');
  await cell.locator('.cell-output-container-webr').waitFor({ timeout: 120_000 });
  if (!/Rows:/.test(await cell.innerText())) throw new Error('Brexit data did not load');

  // Verify editing and running code after startup, then restore the example.
  const editor = cell.locator('.cm-content');
  await editor.fill('mean(c(28, 34, 23, 45))');
  await cell.getByRole('button', { name: 'Run Code', exact: true }).click();
  await cell.getByText('[1] 32.5', { exact: true }).waitFor();
  await cell.getByRole('button', { name: 'Start Over', exact: true }).click();
  return 'PASS: package startup, Brexit data, and editable R execution';
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const url = process.argv[2];
  if (!url) throw new Error('Usage: node scripts/check-webr.mjs <SyntheticControl.html URL>');
  const { chromium } = await import(process.env.PLAYWRIGHT_MODULE || 'playwright');
  const browser = await chromium.launch({ headless: true });
  try {
    console.log(await checkWebR(await browser.newPage(), url));
  } finally {
    await browser.close();
  }
}
