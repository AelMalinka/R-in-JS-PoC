const fs = require('fs');
const { spawn } = require('child_process');

const repo = 'https://cloud.r-project.org';
const lib = 'lib';

if (!fs.existsSync(lib)) {
	fs.mkdirSync(lib);
}

const install = (pkgs) => new Promise((resolve, reject) => {
	const cmd = spawn('R.exe', ['-q', '--vanilla']);
	
	cmd.stderr.on('data', (data) => {
		console.error(`${data}`);
	});
	cmd.stdout.on('data', (data) => {
		console.log(`${data}`);
	});
	
	cmd.on('close', () => {
		resolve();
	});

	pkgs.map((pkg) => {
		cmd.stdin.write(`install.packages("${pkg}", "${lib}", repos="${repo}");`);
	});
	cmd.stdin.end();
});

(async () => {
	await install(process.argv.slice(2));
})().catch(console.error);