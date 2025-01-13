package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.HashMap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DependentServiceHelperTest {

    @InjectMocks
    private DependentServiceHelper dependentServiceHelper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private KafkaProducer kafkaProducer;
    @Mock
    private ITrackingServiceAdapter trackingServiceAdapter;

    private static JsonTestUtility jsonTestUtility;
    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
    }


    @Test
    void pushShipmentDataToDependentService_success() {
        //Test
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        when(kafkaProducer.getKafkaResponse(any(),any(Boolean.class))).thenReturn(new KafkaResponse());
        dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, null);
        verify(kafkaProducer, atLeast(1)).produceToKafka(any(), any(), any());
        verify(kafkaProducer, atLeast(1)).getKafkaResponse(any(), any(Boolean.class));
    }

    @Test
    void pushShipmentDataToDependentServiceCatch() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setStatus(1);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setMasterBill("MBL22");
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(kafkaProducer.getKafkaResponse(any(),any(Boolean.class))).thenReturn(new KafkaResponse());
        when(trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails)).thenReturn(UniversalTrackingPayload.builder().bookingReferenceNumber("1").build());
        when(jsonHelper.convertToJson(any())).thenReturn("json");
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, null);
        verify(kafkaProducer, atLeast(1)).produceToKafka(any(), any(), any());
        verify(kafkaProducer, atLeast(1)).getKafkaResponse(any(), any(Boolean.class));
    }
    @Test
    void pushShipmentDataToDependentServiceSourceNotNull() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getTestShipment();
        shipmentDetails.setStatus(1);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setHouseBill("HBL11");
        shipmentDetails.setMasterBill("MBL22");
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setSource(Constants.API);
        when(kafkaProducer.getKafkaResponse(any(),any(Boolean.class))).thenReturn(new KafkaResponse());
        when(trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails)).thenReturn(UniversalTrackingPayload.builder().bookingReferenceNumber("1").build());
        when(trackingServiceAdapter.mapEventDetailsForTracking(any(), any(), any(), any())).thenReturn(UniversalTrackingPayload.UniversalEventsPayload.builder().build());
        when(jsonHelper.convertToJson(any())).thenReturn("json");
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, null);
        verify(kafkaProducer, atLeast(1)).produceToKafka(any(), any(), any());
        verify(kafkaProducer, atLeast(1)).getKafkaResponse(any(), any(Boolean.class));
    }
}