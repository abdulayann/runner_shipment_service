package com.dpw.runner.shipment.services.filters;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.LoggingConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.impl.GetUserServiceFactory;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import com.dpw.runner.shipment.services.utils.TokenUtility;
import com.nimbusds.jwt.proc.BadJWTException;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.util.Strings;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.text.ParseException;
import java.util.*;

@Component
//@Order(1)
@Slf4j
public class AuthFilter extends OncePerRequestFilter {

    @Autowired
    private GetUserServiceFactory getUserServiceFactory;
    @Autowired
    TokenUtility tokenUtility;
    private static final String VALIDATION_ERROR = "Failed to Validate Auth Token";

    private final String[] ignoredPaths = new String[]{"/actuator/**",
            "/v2/api-docs",
            "/swagger-resources",
            "/swagger-resources/**",
            "/configuration/ui",
            "/configuration/security",
            "/swagger-ui.html",
            "/webjars/**",
            "/api/v2/enums/**"};

    protected boolean shouldNotFilter(HttpServletRequest request) throws ServletException {
        return Arrays.stream(ignoredPaths)
                .anyMatch(e -> new AntPathMatcher().match(e, request.getServletPath()));
    }

    @Override
    public void doFilterInternal(HttpServletRequest servletRequest, HttpServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        try {
        LoggerHelper.putRequestId(UUID.randomUUID().toString());
        HttpServletRequest req = (HttpServletRequest) servletRequest;
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
            user = userService.getUserByToken(authToken);
        } catch (Exception e)
        {
            log.info("Error while validating token with exception: {}", e.getMessage());
            e.printStackTrace();
            res.setContentType("application/json");
            res.setStatus(HttpStatus.UNAUTHORIZED.value());
            return;
        }
        log.info("Time taken to retrieve user definition: {} for request: {}", System.currentTimeMillis() - time, LoggerHelper.getRequestIdFromMDC());

        if (user == null) {
            String errormessage = "Auth failed:- User is not onboarded on shipment service";
            log.info(errormessage);
            res.setContentType("application/json");
            res.setStatus(HttpStatus.UNAUTHORIZED.value());
            //res.getWriter().write(filterLevelException(new UnAuthorizedException(errormessage)));
            return;
        }
        log.debug("Auth Successful, username:-{},tenantId:-{}", user.getUsername(), user.getTenantId());
        UserContext.setUser(user);
        RequestAuthContext.setAuthToken(authToken);
        TenantContext.setCurrentTenant(user.getTenantId());
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
            MDC.remove(LoggingConstants.REQUEST_ID);
            TenantContext.removeTenant();
            RequestAuthContext.removeToken();
            UserContext.removeUser();
        }

    }

    public Collection<? extends GrantedAuthority> getAuthorities(List<String> permissions) {
        List<GrantedAuthority> authorities = new ArrayList<>();
        if(!permissions.isEmpty()) {
            for (String privilege : permissions) {
                authorities.add(new SimpleGrantedAuthority(privilege));
            }
        }
        return authorities;
    }

    private static String getFullURL(HttpServletRequest request) {
        StringBuilder requestURL = new StringBuilder(request.getRequestURI());
        String queryString = request.getQueryString();
        if (queryString == null) {
            return requestURL.toString();
        } else {
            return requestURL.append('?').append(queryString).toString();
        }
    }

    public void writeUnauthorizedResponse(HttpServletResponse res, String errormessage) throws IOException {
        log.info(errormessage);
        res.setContentType("application/json");
        res.setStatus(HttpStatus.UNAUTHORIZED.value());
        //res.getWriter().write(filterLevelException(new UnAuthorizedException(errormessage)));
    }

//    private String filterLevelException(Exception er) throws IOException {
//        BaseResponse baseResponse = new BaseResponse();
//        baseResponse.setError(null);
//        baseResponse.setSuccess(false);
//        baseResponse.setErrorMessage(er.getMessage());
//        return new ObjectMapper().writeValueAsString(baseResponse);
//    }


}

