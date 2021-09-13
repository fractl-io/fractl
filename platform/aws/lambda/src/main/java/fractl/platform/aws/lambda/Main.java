package fractl.platform.aws.lambda;

import fractl.platform.aws.lambda.LambdaHandler;

public class Main {

    public static void main(String[] args) {
        // Test LambdaHandler
        LambdaHandler handler = new LambdaHandler();
        String input = "{\"params\":{\"component\":\"School.Enrollment\"," +
                "\"event\":\"Upsert_Student\"},\"body\":{\"School.Enrollment/Upsert_Student\":" +
                "{\"Instance\":{\"School.Enrollment/Student\":" +
                "{\"FirstName\":\"a\",\"LastName\":\"b\",\"DOB\":\"2014-02-20\"}}}}}";
        System.out.println(handler.handleRequest(input, null));
    }
}
