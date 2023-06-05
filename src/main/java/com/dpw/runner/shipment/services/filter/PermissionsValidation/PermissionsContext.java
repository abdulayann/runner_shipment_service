package com.dpw.runner.shipment.services.filter.PermissionsValidation;

import com.dpw.runner.shipment.services.utility.CommonUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Component
public class PermissionsContext {
    private static ThreadLocal<List<String>> Permissions = new InheritableThreadLocal<>();
    private static List<Object> aclPermissionList = CommonUtils.getAclPermissionList();


    public static List<String> getPermissions() {
        //we need to return Permissions.get()
        return Arrays.asList("airexportfclshipmentList", "airexportlclshipmentList", "seaexportfclshipmentList");
    }

    public static void setPermissions(List<String> UserPermissions) {
        Permissions.set(UserPermissions);
    }
    public static void removePermissions(){
        Permissions.remove();
    }

}