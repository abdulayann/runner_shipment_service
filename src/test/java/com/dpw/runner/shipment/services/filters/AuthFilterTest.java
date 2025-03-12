package com.dpw.runner.shipment.services.filters;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.impl.GetUserServiceFactory;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.impl.UserServiceMavani;
import com.dpw.runner.shipment.services.service.impl.UserServiceV1;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import com.dpw.runner.shipment.services.utils.TokenUtility;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;

import org.apache.catalina.filters.AddDefaultCharsetFilter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.multipart.support.DefaultMultipartHttpServletRequest;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
class AuthFilterTest {

    @Mock
    private IShipmentSettingsDao iShipmentSettingsDao;

    @Mock
    private TenantSettingsService tenantSettingsService;

    @Mock
    private TokenUtility tokenUtility;

    @Mock
    private GetUserServiceFactory getUserServiceFactory;

    @InjectMocks
    private AuthFilter authFilter;

    @Mock
    private JsonHelper jsonHelper;

    @Test
    void testShouldNotFilter() throws ServletException {
        assertFalse(authFilter.shouldNotFilter(new MockHttpServletRequest()));
    }

    @Test
    void testShouldNotFilter2() throws ServletException {
        // Arrange
        DefaultMultipartHttpServletRequest request = mock(DefaultMultipartHttpServletRequest.class);
        when(request.getServletPath()).thenReturn("https://example.org/example");

        boolean actualShouldNotFilterResult = authFilter.shouldNotFilter(request);

        verify(request, atLeast(1)).getServletPath();
        assertFalse(actualShouldNotFilterResult);
    }

    @Test
    void testDoFilterInternal() throws IOException, ServletException {
        // Arrange
        DefaultMultipartHttpServletRequest servletRequest = mock(DefaultMultipartHttpServletRequest.class);
        when(servletRequest.getHeader(Mockito.<String>any())).thenReturn("https://example.org/example");
        when(servletRequest.getRequestURI()).thenReturn("https://example.org/example");
        when(servletRequest.getServletPath()).thenReturn("https://example.org/example");
        AddDefaultCharsetFilter.ResponseWrapper servletResponse = mock(AddDefaultCharsetFilter.ResponseWrapper.class);
        doNothing().when(servletResponse).setStatus(anyInt());
        doNothing().when(servletResponse).setContentType(Mockito.<String>any());
        UserServiceV1 userServiceV1 = new UserServiceV1();
        when(getUserServiceFactory.returnUserService()).thenReturn(userServiceV1);
        authFilter.doFilterInternal(servletRequest, servletResponse, mock(FilterChain.class));

        verify(servletRequest).getHeader("Authorization");
        verify(servletRequest).getRequestURI();
        verify(servletRequest, atLeast(1)).getServletPath();
        verify(servletResponse).setStatus(401);
        verify(servletResponse).setContentType("application/json");
    }

    @Test
    void testDoFilterInternal2() throws IOException, ServletException {

        DefaultMultipartHttpServletRequest servletRequest = mock(DefaultMultipartHttpServletRequest.class);
        when(servletRequest.getHeader(Mockito.<String>any())).thenReturn(null);
        when(servletRequest.getRequestURI()).thenReturn("https://example.org/example");
        when(servletRequest.getServletPath()).thenReturn("https://example.org/example");
        AddDefaultCharsetFilter.ResponseWrapper servletResponse = mock(AddDefaultCharsetFilter.ResponseWrapper.class);
        doNothing().when(servletResponse).setStatus(anyInt());

        authFilter.doFilterInternal(servletRequest, servletResponse, mock(FilterChain.class));

        verify(servletRequest).getHeader("Authorization");
        verify(servletRequest).getRequestURI();
        verify(servletRequest, atLeast(1)).getServletPath();
        verify(servletResponse).setStatus(401);
    }

    @Test
    void testDoFilterInternal3() throws ServletException, IOException {
        DefaultMultipartHttpServletRequest servletRequest = mock(DefaultMultipartHttpServletRequest.class);
        when(servletRequest.getServletPath()).thenReturn("https://example.org/example");
        when(servletRequest.getRequestURI()).thenReturn("https://example.org/example");
        when(servletRequest.getHeader(Mockito.<String>any())).thenReturn("def");
        IUserService userService = mock(IUserService.class);
        when(getUserServiceFactory.returnUserService()).thenReturn(userService);
        UsersDto user = new UsersDto();
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put("tenantAdmin", true);
        permissions.put("superAdmin", false);
        user.setPermissions(permissions);
        user.setTenantId(1);
        when(userService.getUserByToken(any())).thenReturn(user);
        //when(iShipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(new ShipmentSettingsDetails()));
        AddDefaultCharsetFilter.ResponseWrapper servletResponse = mock(AddDefaultCharsetFilter.ResponseWrapper.class);
        authFilter.doFilterInternal(servletRequest, servletResponse, mock(FilterChain.class));
        verify(servletRequest).getHeader("Authorization");
    }

    @Test
    void testDoFilterInternal4() throws ServletException, IOException {
        DefaultMultipartHttpServletRequest servletRequest = mock(DefaultMultipartHttpServletRequest.class);
        when(servletRequest.getServletPath()).thenReturn("https://example.org/example");
        when(servletRequest.getHeader(Mockito.<String>any())).thenReturn("def");
        IUserService userService = mock(IUserService.class);
        when(getUserServiceFactory.returnUserService()).thenReturn(userService);
        when(userService.getUserByToken(any())).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        AddDefaultCharsetFilter.ResponseWrapper servletResponse = mock(AddDefaultCharsetFilter.ResponseWrapper.class);
        authFilter.doFilterInternal(servletRequest, servletResponse, mock(FilterChain.class));
        verify(servletRequest).getHeader("Authorization");
    }
}
