package fractl.platform.aws.lambda;

import java.util.concurrent.atomic.AtomicReference;
import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.amazonaws.services.lambda.runtime.Context;
import clojure.lang.IFn;
import fractl.core;

public class LambdaHandler implements RequestHandler<Object, String> {

    static private AtomicReference evaluator = new AtomicReference();

    @Override
    public String handleRequest(Object input, Context context) {
        IFn result = (IFn)core.process_request(evaluator.get(), input);
        evaluator.compareAndSet(null, result.invoke(1));
        return result.invoke(0).toString();
    }
}