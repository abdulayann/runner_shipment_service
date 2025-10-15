package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.filters.AuthFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@SuppressWarnings("EmptyMethod")
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
@Generated
public class WebSecurityConfig {

    private final AuthFilter authFilter;

    public WebSecurityConfig(AuthFilter authFilter) {
        this.authFilter = authFilter;
    }

    private final String[] ignoredPaths = new String[]
    {
//        "/actuator/**",
//        "/v2/api-docs",
////        "/swagger-resources",
////        "/swagger-resources/**",
//        "/configuration/ui",
//        "/configuration/security",
////        "/swagger-ui.html",
//        "/swagger-ui/**",
//        "/webjars/**",
//        "/migration/consolidation/**",
//        "/api/restore",
//         "/rollback/**",
//        "/api/v2/enums/**",
//        "/api/v2/events/push-tracking-events",
//        "/api/v2/cache/**",
//        "/api/v2/network-transfer/create/external/bridge"
            "/actuator/**",
            "/v2/api-docs",
            "/v3/api-docs",              // add for Springdoc 3.x
            "/v3/api-docs/**",           // add for grouped endpoints
            "/swagger-resources",
            "/swagger-resources/**",
            "/swagger-ui.html",
            "/swagger-ui/**",            // add for index.html + JS
            "/webjars/**",
            "/api-docs/**",
            "/configuration/ui",
            "/configuration/security",
            "/migration/consolidation/**",
            "/api/restore",
            "/rollback/**",
            "/api/v2/enums/**",
            "/api/v2/events/push-tracking-events",
            "/api/v2/cache/**",
            "/api/v2/network-transfer/create/external/bridge",
            "/v3/api-docs/swagger-config"

    };

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity httpSecurity) throws Exception {
        httpSecurity.csrf(AbstractHttpConfigurer::disable)  // Disable CSRF only if API is stateless
                .sessionManagement(session -> session
                        .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                .authorizeRequests()
                .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                .requestMatchers(ignoredPaths).permitAll()
                .anyRequest().authenticated();
        httpSecurity.addFilterBefore(authFilter, UsernamePasswordAuthenticationFilter.class);

        return httpSecurity.build();
    }
}
