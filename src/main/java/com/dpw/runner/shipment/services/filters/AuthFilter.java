package com.dpw.runner.shipment.services.filters;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.*;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentSettingsConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.impl.GetUserServiceFactory;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import com.dpw.runner.shipment.services.utils.TokenUtility;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

@Component
//@Order(1)
@Slf4j
public class AuthFilter extends OncePerRequestFilter {

    public static final String APPLICATION_JSON = "application/json";
    @Autowired
    private GetUserServiceFactory getUserServiceFactory;
    @Autowired
    TokenUtility tokenUtility;
    @Autowired
    IShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    private TenantSettingsService tenantSettingsService;
    @Autowired
    private JsonHelper jsonHelper;

    private final String[] ignoredPaths = new String[]{"/actuator/**",
            "/v2/api-docs",
            "/swagger-resources",
            "/swagger-resources/**",
            "/configuration/ui",
            "/configuration/security",
            "/swagger-ui.html",
            "/webjars/**",
            "/api/v2/enums/**",
            "/api/v2/cache/**"};

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) throws ServletException {
        return Arrays.stream(ignoredPaths)
                .anyMatch(e -> new AntPathMatcher().match(e, request.getServletPath()));
    }

    @Override
    public void doFilterInternal(HttpServletRequest servletRequest, HttpServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        try {
        LoggerHelper.putRequestId(UUID.randomUUID().toString());
        HttpServletRequest req = (HttpServletRequest) servletRequest;
        log.info("Request For Shipment Service API: {} with RequestId: {}",servletRequest.getRequestURI(), LoggerHelper.getRequestIdFromMDC());
        if(shouldNotFilter(req))
        {
            filterChain.doFilter(servletRequest, servletResponse);
            return;
        }
        IUserService userService = getUserServiceFactory.returnUserService();
        HttpServletResponse res = (HttpServletResponse) servletResponse;
        long time = System.currentTimeMillis();
        String authToken = req.getHeader("Authorization");
        if(authToken == null)
        {
            res.setStatus(HttpStatus.UNAUTHORIZED.value());
            return;
        }
        UsersDto user = null;
        try{
            user = userService.getUserByToken(tokenUtility.getUserIdAndBranchId(authToken), authToken);
        } catch (HttpStatusCodeException e)
        {
            log.error("Request: {} || Error while validating token with exception: {} for token: {}", LoggerHelper.getRequestIdFromMDC(), e.getMessage(), authToken);
            e.printStackTrace();
            res.setContentType(APPLICATION_JSON);
            res.setStatus(e.getRawStatusCode());
            return;
        }
        log.info("Time taken to retrieve user definition: {} for request: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());

        if (user == null) {
            String errormessage = "Auth failed:- User is not onboarded on shipment service";
            log.info(errormessage);
            res.setContentType(APPLICATION_JSON);
            res.setStatus(HttpStatus.UNAUTHORIZED.value());
            return;
        }
        log.info("Auth Successful, username:-{},tenantId:-{} for request: {}", user.getUsername(), user.getTenantId(), LoggerHelper.getRequestIdFromMDC());
        UserContext.setUser(user);
        RequestAuthContext.setAuthToken(authToken);
        TenantContext.setCurrentTenant(user.getTenantId());
        //ShipmentSettingsDetailsContext.setCurrentTenantSettings(getTenantSettings());
        List<String> grantedPermissions = new ArrayList<>();
        for (Map.Entry<String,Boolean> entry : user.getPermissions().entrySet())
        {
            if(entry.getValue())
            {
                grantedPermissions.add(entry.getKey());
            }
        }
        UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(
                user, null, getAuthorities(grantedPermissions));
        usernamePasswordAuthenticationToken
                .setDetails(new WebAuthenticationDetailsSource().buildDetails(req));
        PermissionsContext.setPermissions(grantedPermissions);
        SecurityContextHolder.getContext().setAuthentication(usernamePasswordAuthenticationToken);
        filterChain.doFilter(servletRequest, servletResponse);
        double _timeTaken = System.currentTimeMillis() - time;
        log.info(String.format("Request Finished , Total Time in milis:- %s | Request ID: %s", (_timeTaken), LoggerHelper.getRequestIdFromMDC()));
        if (_timeTaken > 500)
            log.info(" RequestId: {} || {} for event: {} Actual time taken: {} ms for API :{}",LoggerHelper.getRequestIdFromMDC(), LoggerEvent.MORE_TIME_TAKEN, LoggerEvent.COMPLETE_API_TIME, _timeTaken, servletRequest.getRequestURI());
        }finally {
            MDC.clear();
            TenantContext.removeTenant();
            RequestAuthContext.removeToken();
            UserContext.removeUser();
            ShipmentSettingsDetailsContext.remove();
            TenantSettingsDetailsContext.remove();
            PermissionsContext.removePermissions();
        }

    }

    private Collection<? extends GrantedAuthority> getAuthorities(List<String> permissions) {
        List<GrantedAuthority> authorities = new ArrayList<>();
        if(!permissions.isEmpty()) {
            for (String privilege : permissions) {
                authorities.add(new SimpleGrantedAuthority(privilege));
            }
        }
        return authorities;
    }

}

