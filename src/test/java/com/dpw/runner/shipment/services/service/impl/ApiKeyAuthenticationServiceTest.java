package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertThrows;

import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ContextConfiguration(classes = {ApiKeyAuthenticationService.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class ApiKeyAuthenticationServiceTest {
    @Autowired
    private ApiKeyAuthenticationService apiKeyAuthenticationService;

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticate() {
        // Arrange, Act and Assert
        assertThrows(UnAuthorizedException.class, () -> apiKeyAuthenticationService.authenticate("Cache", "Api Key"));
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateValid() {
        // Arrange, Act and Assert
        apiKeyAuthenticationService.authenticate("Cache", "93bf1aeb-cadd-42e7-bbf1-0632d93f63e5");
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateInvalidModule() {
        // Arrange, Act and Assert
        apiKeyAuthenticationService.authenticate("Cache1", "93bf1aeb-cadd-42e7-bbf1-0632d93f63e5");
    }
}
