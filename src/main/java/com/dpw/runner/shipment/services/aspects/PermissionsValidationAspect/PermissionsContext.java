package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class PermissionsContext {
    private static ThreadLocal<List<String>> Permissions = new InheritableThreadLocal<>();

    public static List<String> getPermissions() {
        return Permissions.get();
    }

    public static void setPermissions(List<String> UserPermissions) {
        Permissions.set(UserPermissions);
    }
    public static void removePermissions(){
        Permissions.remove();
    }

}