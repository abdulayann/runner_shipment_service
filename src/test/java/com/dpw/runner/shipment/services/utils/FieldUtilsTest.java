package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class FieldUtilsTest {
    /**
     * Method under test: {@link FieldUtils#getNonRelationshipFields(Class)}
     */
    @Test
    void testGetNonRelationshipFields() {
        Class<Containers> clazz = Containers.class;
        List<String> actualNonRelationshipFields = FieldUtils.getNonRelationshipFields(clazz);
        assertTrue(actualNonRelationshipFields.size()>0);
    }
    @Test
    void testGetMasterDataAnnotationFields() {
        List<String> actualMasterDataAnnotationFields = FieldUtils.getMasterDataAnnotationFields(new ArrayList<>());
        assertTrue(actualMasterDataAnnotationFields.isEmpty());
    }

    /**
     * Method under test: {@link FieldUtils#getMasterDataAnnotationFields(List)}
     */
    @Test
    void testGetMasterDataAnnotationFields2() {
        FieldClassDto fieldClassDto = new FieldClassDto();
        Class<ShipmentDetails> clazz = ShipmentDetails.class;
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(null);

        ArrayList<FieldClassDto> classes = new ArrayList<>();
        classes.add(fieldClassDto);
        List<String> actualMasterDataAnnotationFields = FieldUtils.getMasterDataAnnotationFields(classes);
        assertTrue(actualMasterDataAnnotationFields.size() > 0);
    }
    @Test
    void testGetMasterDataAnnotationFieldsWithNonNullParentRef() {
        FieldClassDto fieldClassDto = new FieldClassDto();
        Class<ShipmentDetails> clazz = ShipmentDetails.class;
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef("parentRef.");

        ArrayList<FieldClassDto> classes = new ArrayList<>();
        classes.add(fieldClassDto);
        List<String> actualMasterDataAnnotationFields = FieldUtils.getMasterDataAnnotationFields(classes);
        assertTrue(actualMasterDataAnnotationFields.size() > 0);
    }
    @Test
    void testGetTenantIdAnnotationField() {
        FieldClassDto fieldClassDto = new FieldClassDto();
        Class<ShipmentDetails> clazz = ShipmentDetails.class;
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(null);

        ArrayList<FieldClassDto> classes = new ArrayList<>();
        classes.add(fieldClassDto);
        List<String> actualTenantIdAnnotationFields = FieldUtils.getTenantIdAnnotationFields(classes);
        assertTrue(actualTenantIdAnnotationFields.size() > 0);
    }
}
