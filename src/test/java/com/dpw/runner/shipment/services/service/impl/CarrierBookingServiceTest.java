package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.CarrierBookingMasterDataHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.INotificationService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingUtil;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.ExecutorService;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CarrierBookingServiceTest extends CommonMocks {

    @Mock
    private ICarrierBookingDao carrierBookingDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private CarrierBookingMasterDataHelper carrierBookingMasterDataHelper;
    @Mock
    private CarrierBookingValidationUtil carrierBookingValidationUtil;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private INotificationService notificationService;
    @Mock
    private IV1Service iv1Service;
    @Mock
    private CarrierBookingUtil carrierBookingUtil;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ExecutorService executorServiceMasterData;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;


    @InjectMocks
    private CarrierBookingService carrierBookingService;

    private static CarrierBookingRequest carrierBookingRequest;
    private static CarrierBooking carrierBooking;
    private static ConsolidationDetails consolidationDetails;
    private static CarrierBookingResponse carrierBookingResponse;

    @BeforeAll
    static void init() {
        carrierBookingRequest = new CarrierBookingRequest();
        carrierBookingRequest.setEntityType(Constants.CONSOLIDATION);

        carrierBooking = new CarrierBooking();

        consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(new CarrierDetails());

        carrierBookingResponse = new CarrierBookingResponse();

    }

    @BeforeEach
    void setUp() {
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void createCarrierBookingSuccess(){
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void createCarrierBookingSuccess1(){
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        CarrierBooking carrierBooking1 = new CarrierBooking();
        carrierBooking1.setEntityType(Constants.CONSOLIDATION);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking1);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void updateCarrierBookingNotFound(){
        when(carrierBookingDao.findById(any())).thenThrow(new ValidationException("Not found"));
        assertThrows(ValidationException.class, () -> carrierBookingService.update(carrierBookingRequest));
    }

    @Test
    void updateCarrierBookingSuccess(){
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }

    @Test
    void updateCarrierBookingSuccess1(){
        doNothing().when(carrierBookingValidationUtil).validateServiceType(any());
        when(carrierBookingValidationUtil.validateRequest(any(), any())).thenReturn(new ConsolidationDetails());
        CarrierBooking carrierBooking1 = new CarrierBooking();
        carrierBooking1.setEntityType(Constants.CONSOLIDATION);
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(carrierBooking1);

        when(carrierBookingDao.create(any())).thenReturn(carrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        CarrierBookingResponse response = carrierBookingService.create(carrierBookingRequest);
        assertNotNull(response);
    }
}
