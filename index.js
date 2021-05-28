const R = require('r-script');

process.env.R_LIBS_USER = './lib';

R('./anthro.R').data({}).call((err, res) => {
	if(err) console.log(err.toString());
	else console.dir(res);
});