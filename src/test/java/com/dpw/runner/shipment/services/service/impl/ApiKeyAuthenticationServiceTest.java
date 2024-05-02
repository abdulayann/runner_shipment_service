package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.*;

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
        String inputApiKey = "Api Key";
        // Arrange, Act and Assert
        Exception e = assertThrows(UnAuthorizedException.class, () -> apiKeyAuthenticationService.authenticate("Cache", inputApiKey));
        assertEquals(UnAuthorizedException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateValid() {
        String inputApiKey = "93bf1aeb-cadd-42e7-bbf1-0632d93f63e5";
        // Arrange, Act and Assert
        apiKeyAuthenticationService.authenticate("Cache", inputApiKey);
        assertNotNull(inputApiKey);
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateInvalidModule() {
        String inputApiKey = "93bf1aeb-cadd-42e7-bbf1-0632d93f63e5";
        // Arrange, Act and Assert
        apiKeyAuthenticationService.authenticate("Cache1", inputApiKey);
        assertNotNull(inputApiKey);
    }
}
