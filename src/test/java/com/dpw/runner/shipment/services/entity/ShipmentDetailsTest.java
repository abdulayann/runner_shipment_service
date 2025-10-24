package com.dpw.runner.shipment.services.entity;

import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

public class ShipmentDetailsTest {

    @Test
    public void testShipmentDetails_AssignedTo_SetterAndGetter() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Integer assignedTo = 123;

        shipmentDetails.setAssignedTo(assignedTo);

        assertEquals(assignedTo, shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_Builder() {
        Integer assignedTo = 456;

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .assignedTo(assignedTo)
                .houseBill("HB12345")
                .shipmentId("SH67890")
                .build();

        assertEquals(assignedTo, shipmentDetails.getAssignedTo());
        assertEquals("HB12345", shipmentDetails.getHouseBill());
        assertEquals("SH67890", shipmentDetails.getShipmentId());
    }

    @Test
    public void testShipmentDetails_AssignedTo_Null() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        assertNull(shipmentDetails.getAssignedTo());

        shipmentDetails.setAssignedTo(null);

        assertNull(shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_ChainedSetter() {
        Integer assignedTo = 789;

        ShipmentDetails shipmentDetails = new ShipmentDetails()
                .setAssignedTo(assignedTo)
                .setHouseBill("HB99999")
                .setShipmentId("SH88888");

        assertEquals(assignedTo, shipmentDetails.getAssignedTo());
        assertEquals("HB99999", shipmentDetails.getHouseBill());
        assertEquals("SH88888", shipmentDetails.getShipmentId());
    }

    @Test
    public void testShipmentDetails_AssignedTo_AllArgsConstructor() {
        Integer assignedTo = 999;
        UUID guid = UUID.randomUUID();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .id(1L)
                .guid(guid)
                .parties(null)
                .blDetails(null)
                .carrierDetails(null)
                .measurementDetails(null)
                .houseBill("HB001")
                .transportMode("AIR")
                .direction("IMP")
                .shipmentType("FCL")
                .status(1)
                .source("API")
                .jobType("IMPORT")
                .serviceType("DOOR_TO_DOOR")
                .masterBill("MB001")
                .bookingReference("BK001")
                .consolRef("CR001")
                .salesAgent(1L)
                .paymentTerms("PREPAID")
                .incoterms("FOB")
                .shipmentId("SH001")
                .isDomestic(false)
                .assignedTo(assignedTo)
                .additionalTerms("Additional terms")
                .goodsDescription("Electronic goods")
                .pickupDetails(null)
                .deliveryDetails(null)
                .build();

        assertEquals(1L, shipmentDetails.getId());
        assertEquals(guid, shipmentDetails.getGuid());
        assertEquals("HB001", shipmentDetails.getHouseBill());
        assertEquals("AIR", shipmentDetails.getTransportMode());
        assertEquals("IMP", shipmentDetails.getDirection());
        assertEquals("FCL", shipmentDetails.getShipmentType());
        assertEquals(1, shipmentDetails.getStatus());
        assertEquals("API", shipmentDetails.getSource());
        assertEquals("IMPORT", shipmentDetails.getJobType());
        assertEquals("DOOR_TO_DOOR", shipmentDetails.getServiceType());
        assertEquals("MB001", shipmentDetails.getMasterBill());
        assertEquals("BK001", shipmentDetails.getBookingReference());
        assertEquals("CR001", shipmentDetails.getConsolRef());
        assertEquals(1L, shipmentDetails.getSalesAgent());
        assertEquals("PREPAID", shipmentDetails.getPaymentTerms());
        assertEquals("FOB", shipmentDetails.getIncoterms());
        assertEquals("SH001", shipmentDetails.getShipmentId());
        assertEquals(false, shipmentDetails.getIsDomestic());
        assertEquals(assignedTo, shipmentDetails.getAssignedTo());
        assertEquals("Additional terms", shipmentDetails.getAdditionalTerms());
        assertEquals("Electronic goods", shipmentDetails.getGoodsDescription());
    }

    @Test
    public void testShipmentDetails_AssignedTo_UpdateValue() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        shipmentDetails.setAssignedTo(100);
        assertEquals(100, shipmentDetails.getAssignedTo());

        shipmentDetails.setAssignedTo(200);
        assertEquals(200, shipmentDetails.getAssignedTo());

        shipmentDetails.setAssignedTo(null);
        assertNull(shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_WithOtherFields() {
        Integer assignedTo = 555;

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .houseBill("TEST-HB")
                .transportMode("SEA")
                .direction("EXP")
                .shipmentType("LCL")
                .status(2)
                .assignedTo(assignedTo)
                .build();

        assertNotNull(shipmentDetails);
        assertEquals("TEST-HB", shipmentDetails.getHouseBill());
        assertEquals("SEA", shipmentDetails.getTransportMode());
        assertEquals("EXP", shipmentDetails.getDirection());
        assertEquals("LCL", shipmentDetails.getShipmentType());
        assertEquals(2, shipmentDetails.getStatus());
        assertEquals(assignedTo, shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_ZeroValue() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        shipmentDetails.setAssignedTo(0);

        assertEquals(0, shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_NegativeValue() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        shipmentDetails.setAssignedTo(-1);

        assertEquals(-1, shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_MaxIntValue() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        shipmentDetails.setAssignedTo(Integer.MAX_VALUE);

        assertEquals(Integer.MAX_VALUE, shipmentDetails.getAssignedTo());
    }

    @Test
    public void testShipmentDetails_AssignedTo_MinIntValue() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();

        shipmentDetails.setAssignedTo(Integer.MIN_VALUE);

        assertEquals(Integer.MIN_VALUE, shipmentDetails.getAssignedTo());
    }
}
