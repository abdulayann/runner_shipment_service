package com.dpw.runner.shipment.services.config;


import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.springdoc.core.customizers.OpenApiCustomizer;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Profile({"qa", "dev"})
@Configuration
@Generated
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

    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
                .components(new Components()
                        .addSchemas("RunnerResponse", new Schema<>()
                                .type("object")
                                .addProperty("code", new StringSchema())
                                .addProperty("message", new StringSchema())
                                .addProperty("data", new Schema<>())
                        )
                );
    }

    @Bean
    public GroupedOpenApi shipmentServiceApi(OpenApiCustomizer globalResponseCustomiser) {
        return GroupedOpenApi.builder()
                .group("shipment-service")
                .packagesToScan("com.dpw.runner.shipment")
                .pathsToMatch("/**")
                .addOpenApiCustomizer(globalResponseCustomiser)
                .build();
    }

    @Bean
    public OpenApiCustomizer globalResponseCustomiser() {
        return openApi -> {
            if (openApi.getPaths() != null) {
                openApi.getPaths().values().forEach(pathItem -> {
                    pathItem.readOperationsMap().forEach((httpMethod, operation) -> {
                        io.swagger.v3.oas.models.responses.ApiResponses responses = operation.getResponses();
                        if (responses == null) {
                            responses = new io.swagger.v3.oas.models.responses.ApiResponses();
                            operation.setResponses(responses);
                        }

                        // Add global responses for all HTTP methods
                        io.swagger.v3.oas.models.responses.ApiResponse badRequestResponse = new io.swagger.v3.oas.models.responses.ApiResponse()
                                .description(BAD_REQUEST_MSG)
                                .content(new io.swagger.v3.oas.models.media.Content()
                                        .addMediaType("application/json",
                                                new io.swagger.v3.oas.models.media.MediaType()
                                                        .schema(new Schema<>().$ref("#/components/schemas/RunnerResponse"))
                                        )
                                );
                        responses.addApiResponse("400", badRequestResponse);

                        io.swagger.v3.oas.models.responses.ApiResponse unauthorizedResponse = new io.swagger.v3.oas.models.responses.ApiResponse()
                                .description(NOT_AUTHORIZED)
                                .content(new io.swagger.v3.oas.models.media.Content()
                                        .addMediaType("application/json",
                                                new io.swagger.v3.oas.models.media.MediaType()
                                                        .schema(new Schema<>().$ref("#/components/schemas/RunnerResponse"))
                                        )
                                );
                        responses.addApiResponse("401", unauthorizedResponse);

                        io.swagger.v3.oas.models.responses.ApiResponse forbiddenResponse = new io.swagger.v3.oas.models.responses.ApiResponse()
                                .description(FORBIDDEN_MSG)
                                .content(new io.swagger.v3.oas.models.media.Content()
                                        .addMediaType("application/json",
                                                new io.swagger.v3.oas.models.media.MediaType()
                                                        .schema(new Schema<>().$ref("#/components/schemas/RunnerResponse"))
                                        )
                                );
                        responses.addApiResponse("403", forbiddenResponse);

                        io.swagger.v3.oas.models.responses.ApiResponse notFoundResponse = new io.swagger.v3.oas.models.responses.ApiResponse()
                                .description(NOT_FOUND_MSG)
                                .content(new io.swagger.v3.oas.models.media.Content()
                                        .addMediaType("application/json",
                                                new io.swagger.v3.oas.models.media.MediaType()
                                                        .schema(new Schema<>().$ref("#/components/schemas/RunnerResponse"))
                                        )
                                );
                        responses.addApiResponse("404", notFoundResponse);
                    });
                });
            }
        };
    }

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