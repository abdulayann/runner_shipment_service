package com.dpw.runner.shipment.services.aspects.TimeZoneAspect;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TenantTimeZoneAspectTest {

    private TenantTimeZoneAspect tenantTimeZoneAspect;

    @Mock
    private ProceedingJoinPoint proceedingJoinPoint;

    @BeforeEach
    public void setUp() {
        tenantTimeZoneAspect = new TenantTimeZoneAspect();
    }

    @Test
    void testTenantTimeZoneAspect() throws Throwable {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        ShipmentRequest shipmentRequest = new ShipmentRequest();
        shipmentRequest.setContainersList(new HashSet<>(List.of(new ContainerRequest())));
        shipmentRequest.setCarrierDetails(CarrierDetailRequest.builder().build());
        shipmentRequest.setShipmentCreatedOn(LocalDateTime.now());
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        when(proceedingJoinPoint.getArgs()).thenReturn(new Object[]{request});
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setShipmentCreatedOn(LocalDateTime.now());
        ResponseEntity<?> responseEntity = new ResponseEntity<>(shipmentDetailsResponse, HttpStatus.OK);
        when(proceedingJoinPoint.proceed(any())).thenReturn(responseEntity);
        tenantTimeZoneAspect = new TenantTimeZoneAspect();
        assertDoesNotThrow(() ->tenantTimeZoneAspect.changeTimeZone(proceedingJoinPoint));
    }
}