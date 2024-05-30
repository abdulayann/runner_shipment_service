package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import org.springframework.stereotype.Component;

@Component
public class RequestAuthContext {
    private RequestAuthContext(){}
    private static ThreadLocal<String> currentToken = new InheritableThreadLocal<>();

    public static String getAuthToken() {
        return currentToken.get();
    }

    public static void setAuthToken(String authToken) {
        currentToken.set(authToken);
    }
    public static void removeToken(){
        currentToken.remove();
    }

}