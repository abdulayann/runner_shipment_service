package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.filters.AuthFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@SuppressWarnings("EmptyMethod")
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
@Generated
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

    @Autowired
    private AuthFilter authFilter;

    private final String[] ignoredPaths = new String[]
            {
                    "/actuator/**",
                    "/v2/api-docs",
                    "/swagger-resources",
                    "/swagger-resources/**",
                    "/configuration/ui",
                    "/configuration/security",
                    "/swagger-ui.html",
                    "/webjars/**",
                    "/migration/consolidation/**",
                    "/rollback/**",
                    "/api/v2/enums/**",
                    "/api/v2/events/push-tracking-events",
                    "/api/v2/cache/**"
            };

    @Override
    protected void configure(HttpSecurity httpSecurity) throws Exception {
        httpSecurity.csrf(AbstractHttpConfigurer::disable)  // Disable CSRF only if API is stateless
                .sessionManagement(session -> session
                        .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                .authorizeRequests()
                .antMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                .antMatchers(ignoredPaths).permitAll()
                .anyRequest().authenticated();
        httpSecurity.addFilterBefore(authFilter, UsernamePasswordAuthenticationFilter.class);
    }
}
