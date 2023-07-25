package com.dpw.runner.shipment.services.config;


import com.google.common.collect.ImmutableList;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.config.annotation.*;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.builders.ResponseMessageBuilder;
import springfox.documentation.schema.ModelRef;
import springfox.documentation.service.*;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spi.service.contexts.SecurityContext;
import springfox.documentation.spring.web.paths.RelativePathProvider;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import javax.servlet.ServletContext;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Configuration
@EnableSwagger2
public class SwaggerConfig implements WebMvcConfigurer {

    @Bean
    public Docket swaggerShipmentsApi(ServletContext servletContext) {
        return new Docket(DocumentationType.SWAGGER_2)
                .pathProvider(new RelativePathProvider(servletContext) {
                    @Override
                    public String getApplicationBasePath() {
                        return "/shipment-service";
                    }
                })
                .useDefaultResponseMessages(false)
                .globalResponseMessage(RequestMethod.POST, ImmutableList.of(
                        new ResponseMessageBuilder()
                                .code(400)
                                .message("Bad Request!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(401)
                                .message("Not Authorized!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(403)
                                .message("Forbidden!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(404)
                                .message("Not Found!")
                                .responseModel(new ModelRef("RunnerResponse")).build()
                ))
                .globalResponseMessage(RequestMethod.GET, ImmutableList.of(
                        new ResponseMessageBuilder()
                                .code(400)
                                .message("Bad Request!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(401)
                                .message("Not Authorized!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(403)
                                .message("Forbidden!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(404)
                                .message("Not Found!")
                                .responseModel(new ModelRef("RunnerResponse")).build()
                ))
                .globalResponseMessage(RequestMethod.PUT, ImmutableList.of(
                        new ResponseMessageBuilder()
                                .code(400)
                                .message("Bad Request!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(401)
                                .message("Not Authorized!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(403)
                                .message("Forbidden!")
                                .responseModel(new ModelRef("RunnerResponse")).build(),
                        new ResponseMessageBuilder()
                                .code(404)
                                .message("Not Found!")
                                .responseModel(new ModelRef("RunnerResponse")).build()
                ))
                .select()
                .apis(RequestHandlerSelectors.basePackage("com.dpw.runner.shipment"))
                .paths(PathSelectors.any()).build()
                .securityContexts(Collections.singletonList(securityContext()))
                .securitySchemes(Collections.singletonList(apiKey()))
                .apiInfo(apiInfo());
    }

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