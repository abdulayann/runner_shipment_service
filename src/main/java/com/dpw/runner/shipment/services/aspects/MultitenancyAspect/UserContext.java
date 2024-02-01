package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import org.springframework.stereotype.Component;

@Component
public class UserContext {
    private ThreadLocal<UsersDto> user = new InheritableThreadLocal<>();

    public UsersDto getUser() {
        return user.get();
    }

    public void setUser(UsersDto userId) {
        user.set(userId);
    }

    public void removeUser(){
        user.remove();
    }
}
