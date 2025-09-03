package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FieldUtilsTest {
    /**
     * Method under test: {@link FieldUtils#getNonRelationshipFields(Class)}
     */
    @Test
    void testGetNonRelationshipFields() {
        Class<Containers> clazz = Containers.class;
        List<String> actualNonRelationshipFields = FieldUtils.getNonRelationshipFields(clazz);
        assertFalse(actualNonRelationshipFields.isEmpty());
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
        assertFalse(actualMasterDataAnnotationFields.isEmpty());
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
        assertFalse(actualMasterDataAnnotationFields.isEmpty());
    }
    @Test
    void testGetTenantIdAnnotationField() {
        FieldClassDto fieldClassDto = new FieldClassDto();
        Class<ShipmentDetails> clazz = ShipmentDetails.class;
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(null);

        FieldClassDto fieldClassDto1 = new FieldClassDto();
        Class<AdditionalDetails> clazz1 = AdditionalDetails.class;
        fieldClassDto1.setClazz(clazz1);
        fieldClassDto1.setFieldRef("additionalDetails.");

        ArrayList<FieldClassDto> classes = new ArrayList<>();
        classes.add(fieldClassDto);
        classes.add(fieldClassDto1);
        List<String> actualTenantIdAnnotationFields = FieldUtils.getTenantIdAnnotationFields(classes);
        assertFalse(actualTenantIdAnnotationFields.isEmpty());
    }
}
