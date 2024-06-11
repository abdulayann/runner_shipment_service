package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import org.springframework.stereotype.Component;

import static com.dpw.runner.shipment.services.commons.constants.PermissionConstants.airDG;

@Component
public class UserContext {
    private UserContext(){}
    private static ThreadLocal<UsersDto> user = new InheritableThreadLocal<>();

    public static UsersDto getUser() {
        return user.get();
    }

    public static boolean isDgUser() {
        return getUser().getPermissions().containsKey(airDG)
                && getUser().getPermissions().get(airDG);
    }

    public static void setUser(UsersDto userId) {
        user.set(userId);
    }

    public static void removeUser(){
        user.remove();
    }
}
