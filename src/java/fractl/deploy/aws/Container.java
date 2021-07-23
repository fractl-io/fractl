package fractl.deploy.aws;

import java.util.HashMap;
import java.util.concurrent.CompletableFuture;
import clojure.lang.IFn;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.ecr.EcrAsyncClient;
import software.amazon.awssdk.services.ecr.model.CreateRepositoryRequest;
import software.amazon.awssdk.services.ecr.model.CreateRepositoryResponse;
import software.amazon.awssdk.services.ecr.model.Repository;

public final class Container {
    public static EcrAsyncClient buildClient(String region) {
	return EcrAsyncClient.builder()
	    .region(Region.of(region))
	    .build();
    }

    public static void createRepository(EcrAsyncClient client, String reponame, IFn callback) {
	CreateRepositoryRequest req = CreateRepositoryRequest.builder()
	    .repositoryName(reponame)
	    .build();
	CompletableFuture<CreateRepositoryResponse> future = client.createRepository(req);
	future.whenComplete((reponse, err) -> {
		callback.invoke(reponse, err);
	    });
    }

    public static HashMap<String, String> createRepositoryResponseAsMap(CreateRepositoryResponse response) {
	Repository repo = response.repository();
	HashMap<String, String> result = new HashMap<>();
	result.put("registry-id", repo.registryId());
	result.put("arn", repo.repositoryArn());
	result.put("name", repo.repositoryName());
	result.put("uri", repo.repositoryUri());
	return result;
    }
}
