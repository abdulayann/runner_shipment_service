package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.entity.Containers;
import org.junit.jupiter.api.Test;

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
}
