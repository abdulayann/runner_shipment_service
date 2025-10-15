package com.dpw.runner.shipment.services.config;


import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import io.swagger.v3.oas.models.media.Schema;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import jakarta.servlet.ServletContext;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Profile({"qa", "dev"})
@Configuration
@OpenAPIDefinition(
        info = @Info(
                title = "Shipments API",
                version = "1.0",
                description = "Documentation Shipments API v1.0",
                contact = @Contact(name = "Runner Developers", url = "https://www.dpworld.com/")
        ),
        security = @SecurityRequirement(name = "bearerAuth")
)
@SecurityScheme(
        name = "bearerAuth",
        type = SecuritySchemeType.HTTP,
        scheme = "bearer",
        bearerFormat = "JWT"
)
public class SwaggerConfig implements WebMvcConfigurer {

    public static final String BAD_REQUEST_MSG = "Bad Request!";
    public static final String RUNNER_RESPONSE = "RunnerResponse";
    public static final String NOT_AUTHORIZED = "Not Authorized!";
    public static final String FORBIDDEN_MSG = "Forbidden!";
    public static final String NOT_FOUND_MSG = "Not Found!";

//    @Bean
//    public Docket swaggerShipmentsApi(ServletContext servletContext) {
//        return new Docket(DocumentationType.SWAGGER_2)
//                .pathProvider(new RelativePathProvider(servletContext) {
//                    @Override
//                    public String getApplicationBasePath() {
//                        return "/shipment-service";
//                    }
//                })
//                .useDefaultResponseMessages(false)
//                .globalResponseMessage(RequestMethod.POST, List.of(
//                        new ResponseMessageBuilder()
//                                .code(400)
//                                .message(BAD_REQUEST_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(401)
//                                .message(NOT_AUTHORIZED)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(403)
//                                .message(FORBIDDEN_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(404)
//                                .message(NOT_FOUND_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build()
//                ))
//                .globalResponseMessage(RequestMethod.GET, List.of(
//                        new ResponseMessageBuilder()
//                                .code(400)
//                                .message(BAD_REQUEST_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(401)
//                                .message(NOT_AUTHORIZED)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(403)
//                                .message(FORBIDDEN_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(404)
//                                .message(NOT_FOUND_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build()
//                ))
//                .globalResponseMessage(RequestMethod.PUT, List.of(
//                        new ResponseMessageBuilder()
//                                .code(400)
//                                .message(BAD_REQUEST_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(401)
//                                .message(NOT_AUTHORIZED)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(403)
//                                .message(FORBIDDEN_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build(),
//                        new ResponseMessageBuilder()
//                                .code(404)
//                                .message(NOT_FOUND_MSG)
//                                .responseModel(new ModelRef(RUNNER_RESPONSE)).build()
//                ))
//                .select()
//                .apis(RequestHandlerSelectors.basePackage("com.dpw.runner.shipment"))
//                .paths(PathSelectors.any()).build()
//                .securityContexts(Collections.singletonList(securityContext()))
//                .securitySchemes(Collections.singletonList(apiKey()))
//                .apiInfo(apiInfo());
//    }
//
//    private ApiInfo apiInfo() {
//        return new ApiInfoBuilder()
//                .version("1.0")
//                .title("Shipments API")
//                .description("Documentation Shipments API v1.0")
//                .contact(new Contact("Runner Developers", "https://www.dpworld.com/", "")).build();
//    }
//
//    public ApiKey apiKey() {
//        return new ApiKey("JWT", "Authorization", "header");
//    }
//
//    private SecurityContext securityContext() {
//        return SecurityContext.builder().securityReferences(defaultAuth()).forPaths(PathSelectors.any()).build();
//    }
//
//    private List<SecurityReference> defaultAuth() {
//        AuthorizationScope authorizationScope = new AuthorizationScope("global", "accessEverything");
//        AuthorizationScope[] scopes = new AuthorizationScope[1];
//
//        scopes[0] = authorizationScope;
//        SecurityReference reference = new SecurityReference("JWT", scopes);
//        List<SecurityReference> auths = new ArrayList<>();
//        auths.add(reference);
//        return auths;
//    }
//@Bean
//public OpenApiCustomiser globalResponseCustomiser() {
//    return openApi -> openApi.getPaths().values().forEach(pathItem ->
//            pathItem.readOperations().forEach(operation -> {
//                ApiResponses responses = operation.getResponses();
//
//                // Define shared schema for error (optional: create class ErrorResponse and generate schema)
//                Schema<?> errorSchema = new Schema<>().type("object");
//
//                Content content = new Content()
//                        .addMediaType(APPLICATION_JSON_VALUE, new MediaType().schema(errorSchema));
//
//                // Add 400, 404, 500 responses if not present
//                if (!responses.containsKey("400")) {
//                    responses.addApiResponse("400",
//                            new ApiResponse().description("Bad Request").content(content));
//                }
//                if (!responses.containsKey("404")) {
//                    responses.addApiResponse("404",
//                            new ApiResponse().description("Not Found").content(content));
//                }
//                if (!responses.containsKey("500")) {
//                    responses.addApiResponse("500",
//                            new ApiResponse().description("Internal Server Error").content(content));
//                }
//            })
//    );
//}

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("swagger-ui.html")
                .addResourceLocations("classpath:/META-INF/resources/");

        registry.addResourceHandler("/webjars/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/");
        registry.addResourceHandler("/swagger-ui/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/springfox-swagger-ui/");

    }

}