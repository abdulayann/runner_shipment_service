package com.dpw.runner.shipment.services.config;


import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.config.annotation.*;
import springfox.documentation.builders.*;
import springfox.documentation.schema.Example;
import springfox.documentation.schema.ModelRef;
import springfox.documentation.service.*;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spi.service.contexts.SecurityContext;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import javax.servlet.ServletContext;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Configuration
public class SwaggerConfig implements WebMvcConfigurer {

    public static final String BAD_REQUEST_MSG = "Bad Request!";
    public static final String RUNNER_RESPONSE = "RunnerResponse";
    public static final String NOT_AUTHORIZED = "Not Authorized!";
    public static final String FORBIDDEN_MSG = "Forbidden!";
    public static final String NOT_FOUND_MSG = "Not Found!";


    @Bean
    public Docket api() {
        return new Docket(DocumentationType.SWAGGER_2)
                .useDefaultResponseMessages(false)
//                .globalResponses(HttpMethod.POST, List.of(
//                        new ResponseBuilder()
//                                .code("400")
//                                .description(BAD_REQUEST_MSG)
//                                .examples(List.of(new Example("1", BAD_REQUEST_MSG, BAD_REQUEST_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("401")
//                                .description(NOT_AUTHORIZED)
//                                .examples(List.of(new Example("1", NOT_AUTHORIZED, NOT_AUTHORIZED, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("403")
//                                .description(FORBIDDEN_MSG)
//                                .examples(List.of(new Example("1", FORBIDDEN_MSG, FORBIDDEN_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("404")
//                                .description(NOT_FOUND_MSG)
//                                .examples(List.of(new Example("1", NOT_FOUND_MSG, NOT_FOUND_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build()
//                ))
//                .globalResponses(HttpMethod.GET, List.of(
//                        new ResponseBuilder()
//                                .code("400")
//                                .description(BAD_REQUEST_MSG)
//                                .examples(List.of(new Example("1", BAD_REQUEST_MSG, BAD_REQUEST_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("401")
//                                .description(NOT_AUTHORIZED)
//                                .examples(List.of(new Example("1", NOT_AUTHORIZED, NOT_AUTHORIZED, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("403")
//                                .description(FORBIDDEN_MSG)
//                                .examples(List.of(new Example("1", FORBIDDEN_MSG, FORBIDDEN_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("404")
//                                .description(NOT_FOUND_MSG)
//                                .examples(List.of(new Example("1", NOT_FOUND_MSG, NOT_FOUND_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build()
//                ))
//                .globalResponses(HttpMethod.PUT, List.of(
//                        new ResponseBuilder()
//                                .code("400")
//                                .description(BAD_REQUEST_MSG)
//                                .examples(List.of(new Example("1", BAD_REQUEST_MSG, BAD_REQUEST_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("401")
//                                .description(NOT_AUTHORIZED)
//                                .examples(List.of(new Example("1", NOT_AUTHORIZED, NOT_AUTHORIZED, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("403")
//                                .description(FORBIDDEN_MSG)
//                                .examples(List.of(new Example("1", FORBIDDEN_MSG, FORBIDDEN_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("404")
//                                .description(NOT_FOUND_MSG)
//                                .examples(List.of(new Example("1", NOT_FOUND_MSG, NOT_FOUND_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE)))
//                                .build()
//                ))
                .select()
                .apis(RequestHandlerSelectors.basePackage("com.dpw.runner.shipment"))
                .paths(PathSelectors.any()).build()
                .securityContexts(Collections.singletonList(securityContext()))
                .securitySchemes(Collections.singletonList(apiKey()))
                .apiInfo(apiInfo());
    }

//    @Bean
//    public Docket swaggerShipmentsApi(ServletContext servletContext) {
//        return new Docket(DocumentationType.SWAGGER_2)
////                .pathProvider(new RelativePathProvider(servletContext) {
////                    @Override
////                    public String getApplicationBasePath() {
////                        return "/shipment-service";
////                    }
////                })
//                .useDefaultResponseMessages(false)
//                .globalResponses(HttpMethod.POST, List.of(
//                        new ResponseBuilder()
//                                .code("400")
//                                .description(BAD_REQUEST_MSG)
//                                .examples(List.of(new Example("1", BAD_REQUEST_MSG, BAD_REQUEST_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("401")
//                                .description(NOT_AUTHORIZED)
//                                .examples(List.of(new Example("1", NOT_AUTHORIZED, NOT_AUTHORIZED, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("403")
//                                .description(FORBIDDEN_MSG)
//                                .examples(List.of(new Example("1", FORBIDDEN_MSG, FORBIDDEN_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("404")
//                                .description(NOT_FOUND_MSG)
//                                .examples(List.of(new Example("1", NOT_FOUND_MSG, NOT_FOUND_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build()
//                ))
//                .globalResponses(HttpMethod.GET, List.of(
//                        new ResponseBuilder()
//                                .code("400")
//                                .description(BAD_REQUEST_MSG)
//                                .examples(List.of(new Example("1", BAD_REQUEST_MSG, BAD_REQUEST_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("401")
//                                .description(NOT_AUTHORIZED)
//                                .examples(List.of(new Example("1", NOT_AUTHORIZED, NOT_AUTHORIZED, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("403")
//                                .description(FORBIDDEN_MSG)
//                                .examples(List.of(new Example("1", FORBIDDEN_MSG, FORBIDDEN_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("404")
//                                .description(NOT_FOUND_MSG)
//                                .examples(List.of(new Example("1", NOT_FOUND_MSG, NOT_FOUND_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build()
//                ))
//                .globalResponses(HttpMethod.PUT, List.of(
//                        new ResponseBuilder()
//                                .code("400")
//                                .description(BAD_REQUEST_MSG)
//                                .examples(List.of(new Example("1", BAD_REQUEST_MSG, BAD_REQUEST_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("401")
//                                .description(NOT_AUTHORIZED)
//                                .examples(List.of(new Example("1", NOT_AUTHORIZED, NOT_AUTHORIZED, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("403")
//                                .description(FORBIDDEN_MSG)
//                                .examples(List.of(new Example("1", FORBIDDEN_MSG, FORBIDDEN_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build(),
//                        new ResponseBuilder()
//                                .code("404")
//                                .description(NOT_FOUND_MSG)
//                                .examples(List.of(new Example("1", NOT_FOUND_MSG, NOT_FOUND_MSG, new RunnerResponse(), null, MediaType.APPLICATION_JSON_VALUE))).build()
//                ))
//                .select()
//                .apis(RequestHandlerSelectors.basePackage("com.dpw.runner.shipment"))
//                .paths(PathSelectors.any()).build()
//                .securityContexts(Collections.singletonList(securityContext()))
//                .securitySchemes(Collections.singletonList(apiKey()))
//                .apiInfo(apiInfo());
//    }

    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .version("1.0")
                .title("Shipments API")
                .description("Documentation Shipments API v1.0")
                .contact(new Contact("Runner Developers", "https://www.dpworld.com/", "")).build();
    }

    public ApiKey apiKey() {
        return new ApiKey("JWT", "Authorization", "header");
    }

    private SecurityContext securityContext() {
        return SecurityContext.builder().securityReferences(defaultAuth()).forPaths(PathSelectors.any()).build();
    }

    private List<SecurityReference> defaultAuth() {
        AuthorizationScope authorizationScope = new AuthorizationScope("global", "accessEverything");
        AuthorizationScope[] scopes = new AuthorizationScope[1];

        scopes[0] = authorizationScope;
        SecurityReference reference = new SecurityReference("JWT", scopes);
        List<SecurityReference> auths = new ArrayList<>();
        auths.add(reference);
        return auths;
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("swagger-ui.html")
                .addResourceLocations("classpath:/META-INF/resources/");

        registry.addResourceHandler("/webjars/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/");
    }

}