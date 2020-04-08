const path = require('path');

const { google } = require('googleapis');

const keys = JSON.parse(process.env.OAUTH_CREDS).installed;

const oauth2Client = new google.auth.OAuth2(
  keys.client_id,
  keys.client_secret,
  keys.redirect_uris[0]
);

google.options({ auth: oauth2Client });

const scope = [
  'https://www.googleapis.com/auth/cloud-platform',
  'https://www.googleapis.com/auth/monitoring',
  'https://www.googleapis.com/auth/monitoring.read',
].join(' ');

const getCodeVerifierAndUrl = async () => {
  const codes = await oauth2Client.generateCodeVerifierAsync();
  const url = oauth2Client.generateAuthUrl({
    access_type: 'offline',
    scope,
    // When using `generateCodeVerifier`, make sure to use code_challenge_method 'S256'.
    code_challenge_method: 'S256',
    // Pass along the generated code challenge.
    code_challenge: codes.codeChallenge,
  });
  return { codes, url };
};

var argv = process.argv.slice(2);
var cmd = argv[0];
var refresh_token = process.env.REFRESH_TOKEN;

const kickoff = async () => {
  if (cmd === 'login') {
    const { codes, url } = await getCodeVerifierAndUrl();
    console.log(url);
    console.log(codes.codeVerifier);
  } else if (cmd === 'verify') {
    var verifier = argv[1];
    var code = argv[2];
    const { tokens } = await oauth2Client.getToken({
      code,
      codeVerifier: verifier,
    });
    oauth2Client.setCredentials(tokens);
    if (!tokens.refresh_token) {
      throw new Error('Expected a refresh_token in the response, got null (do you need to deauthorize the app?)');
    }
    console.log(tokens.refresh_token);
  } else if (refresh_token == null) {
    throw new Error('REFRESH_TOKEN must be set.');
  } else {
    oauth2Client.on('tokens', ({ access_token }) => console.log(access_token));
    await oauth2Client.refreshToken(refresh_token);
  }
};

kickoff().catch(console.log);


