package fractl.auth.auth0;

import com.auth0.client.auth.AuthAPI;
import com.auth0.client.auth.AuthorizeUrlBuilder;
import java.util.Arrays;

public class Auth0AuthUtil {

    public static String authorizeUrl(String clientId, String clientSecret, String authDomain,
                                      String authCallbackUrl, String[] scope) {
        
        AuthAPI auth = new AuthAPI(authDomain, clientId, clientSecret);
        auth.setLoggingEnabled(true);

        String authUrl = auth.authorizeUrl(authCallbackUrl).withScope(Arrays.toString(scope)).build();

        return authUrl;
    }
}
