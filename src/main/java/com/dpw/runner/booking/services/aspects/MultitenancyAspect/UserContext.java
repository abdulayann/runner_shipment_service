package com.dpw.runner.booking.services.aspects.MultitenancyAspect;

import com.dpw.runner.booking.services.dto.request.UsersDto;
import org.springframework.stereotype.Component;

import static com.dpw.runner.booking.services.commons.constants.PermissionConstants.*;

@Component
public class UserContext {
    private UserContext(){}
    private static ThreadLocal<UsersDto> user = new InheritableThreadLocal<>();

    public static UsersDto getUser() {
        return user.get();
    }

    public static boolean isAirDgUser() {
        return getUser().getPermissions().containsKey(airDG)
                && getUser().getPermissions().get(airDG);
    }

    public static boolean isOceanDgUser() {
        return getUser().getPermissions().containsKey(OCEAN_DG_APPROVER)
                && getUser().getPermissions().get(OCEAN_DG_APPROVER);
    }

    public static boolean isOceanDgCommercialUser() {
        return getUser().getPermissions().containsKey(OCEAN_DG_COMMERCIAL_APPROVER)
                && getUser().getPermissions().get(OCEAN_DG_COMMERCIAL_APPROVER);
    }

    public static void setUser(UsersDto userId) {
        user.set(userId);
    }

    public static void removeUser(){
        user.remove();
    }
}
