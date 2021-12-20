package fractl.auth.auth0;

import com.auth0.client.auth.AuthAPI;
import com.auth0.client.auth.AuthorizeUrlBuilder;
import com.auth0.json.auth.TokenHolder;
import com.auth0.exception.Auth0Exception;

public class Auth0AuthUtil {

    public static String authorizeUrl(String clientId, String clientSecret, String authDomain,
                                      String authCallbackUrl, String scope) {
        
        AuthAPI auth = new AuthAPI(authDomain, clientId, clientSecret);
        auth.setLoggingEnabled(true);

        String authUrl = auth.authorizeUrl(authCallbackUrl).withScope(scope).build();

        return authUrl;
    }

	public static TokenHolder PasswordLogin(String clientId, String clientSecret, String authDomain,
											String userName, String password, String scope) {

        AuthAPI auth = new AuthAPI(authDomain, clientId, clientSecret);
        auth.setLoggingEnabled(true);

		try {
			TokenHolder result = auth.login(userName, password.toCharArray()).setScope(scope).execute();
			return result;
		} catch (Auth0Exception ex) {
			ex.printStackTrace();
			return null;
		}
	}

}
