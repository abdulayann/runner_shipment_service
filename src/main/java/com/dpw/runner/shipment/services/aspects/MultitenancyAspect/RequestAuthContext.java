package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import org.springframework.stereotype.Component;

@Component
public class RequestAuthContext {
    private ThreadLocal<String> currentToken = new InheritableThreadLocal<>();

    public String getAuthToken() {
        return currentToken.get();
    }

    public void setAuthToken(String authToken) {
        currentToken.set(authToken);
    }
    public void removeToken(){
        currentToken.remove();
    }

}