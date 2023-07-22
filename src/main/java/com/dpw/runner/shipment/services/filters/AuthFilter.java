package com.dpw.runner.shipment.services.filters;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
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
import org.springframework.stereotype.Component;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Component
@Order(1)
@Slf4j
public class AuthFilter implements Filter {

    @Autowired
    private GetUserServiceFactory getUserServiceFactory;
    @Autowired
    TokenUtility tokenUtility;
    private static final String VALIDATION_ERROR = "Failed to Validate Auth Token";


    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {

        IUserService userService = getUserServiceFactory.returnUserService();
        HttpServletRequest req = (HttpServletRequest) servletRequest;
        HttpServletResponse res = (HttpServletResponse) servletResponse;
        long time = System.currentTimeMillis();
        String authToken = req.getHeader("Authorization");
        if(authToken == null)
        {
            res.setStatus(HttpStatus.FORBIDDEN.value());
            return;
        }

        UsersDto user = userService.getUserByToken(authToken);
        if (user == null) {
            String errormessage = "Auth failed:- User is not onboarded on shipment service";
            log.info(errormessage);
            res.setContentType("application/json");
            res.setStatus(HttpStatus.FORBIDDEN.value());
            //res.getWriter().write(filterLevelException(new UnAuthorizedException(errormessage)));
            return;
        }
        log.debug("Auth Successful, username:-{},tenantId:-{}", user.getUsername(), user.getTenantId());
        UserContext.setUser(user);
        TenantContext.setCurrentTenant(user.getTenantId());
        List<String> grantedPermissions = new ArrayList<>();
        for (Map.Entry<String,Boolean> entry : user.getPermissions().entrySet())
        {
            if(entry.getValue())
            {
                grantedPermissions.add(entry.getKey());
            }
        }
        PermissionsContext.setPermissions(grantedPermissions);
        MDC.put("request_id", UUID.randomUUID().toString());
        try {
            filterChain.doFilter(servletRequest, servletResponse);
            log.info(String.format("Request Finished , Total Time in milis:- %s", (System.currentTimeMillis() - time)));
        }finally {
            MDC.remove("request_id");
            TenantContext.removeTenant();
            RequestAuthContext.removeToken();
            UserContext.removeUser();
        }

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

