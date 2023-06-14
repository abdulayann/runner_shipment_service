package com.dpw.runner.shipment.services.filter.Multitenancy;

import com.dpw.runner.shipment.services.dto.UsersDto;
import org.springframework.stereotype.Component;

@Component
public class UserContext {
    private static ThreadLocal<UsersDto> user = new InheritableThreadLocal<>();

    public static UsersDto getUser() {
        return user.get();
    }

    public static void setUser(UsersDto userId) {
        user.set(userId);
    }

    public static void removeUser(){
        user.remove();
    }
}
