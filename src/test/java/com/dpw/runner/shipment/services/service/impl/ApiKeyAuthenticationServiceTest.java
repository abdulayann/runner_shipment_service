package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ContextConfiguration(classes = {ApiKeyAuthenticationService.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
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
        apiKeyAuthenticationService.authenticate("Cache", inputApiKey);
        assertNotNull(inputApiKey);
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateValidTrackingPushApi() {
        String inputApiKey = "286b0dbd-2ac8-482b-bf24-0b15df02e873";
        apiKeyAuthenticationService.authenticate(Constants.TRACKING_PUSH_API, inputApiKey);
        assertNotNull(inputApiKey);
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateInvalidTrackingPushApi() {
        String inputApiKey = "invalid-key";
        Exception e = assertThrows(UnAuthorizedException.class,
                () -> apiKeyAuthenticationService.authenticate(Constants.TRACKING_PUSH_API, inputApiKey));
        assertEquals("UnAuthorizedException", e.getClass().getSimpleName());
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateValidMigrationApi() {
        String inputApiKey = "49deefb5-599f-4563-817e-f875a2680b10";
        apiKeyAuthenticationService.authenticate(Constants.MIGRATION_API, inputApiKey);
        assertNotNull(inputApiKey);
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateInvalidMigrationApi() {
        String inputApiKey = "wrong-key";
        Exception e = assertThrows(UnAuthorizedException.class,
                () -> apiKeyAuthenticationService.authenticate(Constants.MIGRATION_API, inputApiKey));
        assertEquals("UnAuthorizedException", e.getClass().getSimpleName());
    }

    /**
     * Method under test:
     * {@link ApiKeyAuthenticationService#authenticate(String, String)}
     */
    @Test
    void testAuthenticateWithUnknownModule_shouldThrowException() {
        String inputApiKey = "some-key";
        Exception e = assertThrows(UnAuthorizedException.class,
                () -> apiKeyAuthenticationService.authenticate("UNKNOWN_MODULE", inputApiKey));
        assertEquals("UnAuthorizedException", e.getClass().getSimpleName());
    }

}
