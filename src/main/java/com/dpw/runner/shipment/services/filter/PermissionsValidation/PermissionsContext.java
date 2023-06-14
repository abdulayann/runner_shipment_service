package com.dpw.runner.shipment.services.filter.PermissionsValidation;

import com.dpw.runner.shipment.services.service.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Component
public class PermissionsContext {
    private static ThreadLocal<List<String>> Permissions = new InheritableThreadLocal<>();

    @Autowired
    private static IUserService usersService;

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