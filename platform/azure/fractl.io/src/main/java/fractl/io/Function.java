package fractl.io;

import java.util.concurrent.atomic.AtomicReference;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.HttpMethod;
import com.microsoft.azure.functions.HttpRequestMessage;
import com.microsoft.azure.functions.HttpResponseMessage;
import com.microsoft.azure.functions.HttpStatus;
import com.microsoft.azure.functions.annotation.AuthorizationLevel;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.HttpTrigger;

import java.util.Optional;
import clojure.lang.IFn;
import fractl.core;

/**
 * Azure Functions with HTTP Trigger.
 */
public class Function {

    static private AtomicReference evaluator = new AtomicReference();
    /**
     * This function listens at endpoint "/api/fractl". Two ways to invoke it using "curl" command in bash:
     *   curl -d "HTTP Body" {your host}/api/fractl
     */
    @FunctionName("fractl")
    public HttpResponseMessage run(
            @HttpTrigger(
                name = "req",
                methods = {HttpMethod.GET, HttpMethod.POST},
                authLevel = AuthorizationLevel.ANONYMOUS)
                HttpRequestMessage<Optional<String>> request,
            final ExecutionContext context) {
	context.getLogger().info("Java HTTP trigger processed a request.");
	String input = request.getBody().orElse("");
        context.getLogger().info("RequestBody:- " + input);
        IFn result = (IFn)core.process_request(evaluator.get(), input);
        evaluator.compareAndSet(null, result.invoke(1));
        String output = result.invoke(0).toString();
	return request.createResponseBuilder(HttpStatus.OK).body(output).build();
        // context.getLogger().info("Java HTTP trigger processed a request.");

        // // Parse query parameter
        // final String query = request.getQueryParameters().get("name");
        // final String name = request.getBody().orElse(query);

        // if (name == null) {
        //     return request.createResponseBuilder(HttpStatus.BAD_REQUEST).body("Please pass a name on the query string or in the request body").build();
        // } else {
        //     return request.createResponseBuilder(HttpStatus.OK).body("Hello, " + name).build();
        // }
    }
}
