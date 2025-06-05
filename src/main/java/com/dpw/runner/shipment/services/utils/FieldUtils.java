package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.response.FieldClassDto;

import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public class FieldUtils {
    private FieldUtils() {
    }

    /**
     * Gets all fields of a class except Hibernate relationship fields.
     *
     * @param clazz The class to inspect.
     * @return A list of fields excluding Hibernate relationship fields.
     */
    public static List<String> getNonRelationshipFields(Class<?> clazz) {
        List<String> fields = new ArrayList<>();

        // Get all declared fields (including private ones)
        for (Field field : clazz.getDeclaredFields()) {
            // Check if the field has any Hibernate relationship annotation
            if (!isRelationshipField(field)) {
                fields.add(field.getName());
            }
        }

        return fields;
    }

    private static boolean isRelationshipField(Field field) {
        return field.isAnnotationPresent(OneToOne.class) ||
                field.isAnnotationPresent(OneToMany.class) ||
                field.isAnnotationPresent(ManyToOne.class) ||
                field.isAnnotationPresent(ManyToMany.class);
    }

    public static List<String> getMasterDataAnnotationFields(List<FieldClassDto> classes) {
        List<String> fields = new ArrayList<>();
        // Get all declared fields (including private ones)
        for (FieldClassDto fieldClassDto : classes) {
            for (Field field : fieldClassDto.getClazz().getDeclaredFields()) {
                // Check if the field has any Hibernate relationship annotation
                if (isMasterDataField(field)) {
                    if (StringUtility.isNotEmpty(fieldClassDto.getFieldRef())) {
                        fields.add(fieldClassDto.getFieldRef() + field.getName());
                    } else {
                        fields.add(field.getName());
                    }
                }
            }
        }
        return fields;
    }

    private static boolean isMasterDataField(Field field) {
        return field.isAnnotationPresent(MasterData.class) ||
                field.isAnnotationPresent(DedicatedMasterData.class) || field.isAnnotationPresent(UnlocationData.class) ||
                field.isAnnotationPresent(OrganizationMasterData.class);
    }

    public static List<String> getTenantIdAnnotationFields(List<FieldClassDto> classes) {
        List<String> fields = new ArrayList<>();
        // Get all declared fields (including private ones)
        for (FieldClassDto fieldClassDto : classes) {
            for (Field field : fieldClassDto.getClazz().getDeclaredFields()) {
                // Check if the field has any Hibernate relationship annotation
                if (isTenantIdDataField(field)) {
                    fields.add(field.getName());
                }
            }
        }
        return fields;
    }

    private static boolean isTenantIdDataField(Field field) {
        return field.isAnnotationPresent(TenantIdData.class);
    }
}
