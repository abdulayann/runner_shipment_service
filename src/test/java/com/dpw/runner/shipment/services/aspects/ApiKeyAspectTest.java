package com.dpw.runner.shipment.services.aspects;

import com.dpw.runner.shipment.services.authservice.ApiKeyService;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ApiKeyAspectTest {

    @InjectMocks
    private ApiKeyAspect apiKeyAspect;

    @Mock
    private ApiKeyService apiKeyService;

    @Mock
    private ProceedingJoinPoint joinPoint;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testValidApiKey_AllowsProceed() throws Throwable {
        // Arrange
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("x-api-key", "valid-key");
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));

        when(apiKeyService.isValidApiKey("valid-key")).thenReturn(true);
        when(joinPoint.proceed()).thenReturn("success");

        // Act
        Object result = apiKeyAspect.validateApiKey(joinPoint);

        // Assert
        assertEquals("success", result);
        verify(apiKeyService).isValidApiKey("valid-key");
        verify(joinPoint).proceed();
    }

    @Test
    void testMissingRequest_ThrowsException() {
        // Arrange
        RequestContextHolder.resetRequestAttributes();

        // Act & Assert
        UnAuthorizedException ex = assertThrows(UnAuthorizedException.class,
                () -> apiKeyAspect.validateApiKey(joinPoint));

        assertEquals("Cannot access request", ex.getMessage());
    }

    @Test
    void testMissingApiKey_ThrowsException() {
        // Arrange
        MockHttpServletRequest request = new MockHttpServletRequest();
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));

        // Act & Assert
        UnAuthorizedException ex = assertThrows(UnAuthorizedException.class,
                () -> apiKeyAspect.validateApiKey(joinPoint));

        assertEquals("Valid API key required", ex.getMessage());
    }

    @Test
    void testInvalidApiKey_ThrowsException() throws Throwable {
        // Arrange
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("x-api-key", "invalid-key");
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));

        when(apiKeyService.isValidApiKey("invalid-key")).thenReturn(false);

        // Act & Assert
        UnAuthorizedException ex = assertThrows(UnAuthorizedException.class,
                () -> apiKeyAspect.validateApiKey(joinPoint));

        assertEquals("Valid API key required", ex.getMessage());
        verify(apiKeyService).isValidApiKey("invalid-key");
        verify(joinPoint, never()).proceed();
    }
}
