package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;
import org.springframework.http.ResponseEntity;

import java.lang.reflect.InvocationTargetException;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

@ExtendWith(MockitoExtension.class)
class TruckDriverDetailsServiceTest {

    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @InjectMocks
    private TruckDriverDetailsService truckDriverDetailsService;


    @Test
    public void testCreate_Successful() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TruckDriverDetailsRequest request = TruckDriverDetailsRequest.builder().id(12L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        TruckDriverDetails truckDriverDetails = TruckDriverDetails.builder().build();

        Mockito.when(jsonHelper.convertValue(any(), Mockito.eq(TruckDriverDetails.class))).thenReturn(truckDriverDetails);
        Mockito.when(truckDriverDetailsDao.save(any())).thenReturn(truckDriverDetails);

        var resp = truckDriverDetailsService.create(commonRequestModel);

        Mockito.verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
        Mockito.verify(truckDriverDetailsDao, times(1)).save(any(TruckDriverDetails.class));

        Assertions.assertNotNull(resp.getBody());
    }


    @Test
    public void testUpdate_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CommonRequestModel commonRequestModel = Mockito.mock(CommonRequestModel.class);
        TruckDriverDetailsRequest request = Mockito.mock(TruckDriverDetailsRequest.class);
        Mockito.when(commonRequestModel.getData()).thenReturn(request);
        Mockito.when(request.getId()).thenReturn(1L);

        TruckDriverDetails oldEntity = Mockito.mock(TruckDriverDetails.class);
        oldEntity.setId(12L);
        Optional<TruckDriverDetails> optionalOldEntity = Optional.of(oldEntity);
        Mockito.when(truckDriverDetailsDao.findById(Mockito.anyLong())).thenReturn(optionalOldEntity);
        Mockito.when(jsonHelper.convertValue(any(TruckDriverDetailsRequest.class), eq(TruckDriverDetails.class))).thenReturn(oldEntity);
        TruckDriverDetails truckDriverDetails = Mockito.mock(TruckDriverDetails.class);
        Mockito.when(truckDriverDetailsDao.save(any(TruckDriverDetails.class))).thenReturn(truckDriverDetails);

        ResponseEntity<IRunnerResponse> responseEntity = truckDriverDetailsService.update(commonRequestModel);

        Mockito.verify(truckDriverDetailsDao, times(1)).findById(Mockito.anyLong());
        Mockito.verify(truckDriverDetailsDao, times(1)).save(any(TruckDriverDetails.class));
        Mockito.verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    public void testList_Success() {
        // Given
        CommonRequestModel commonRequestModel = Mockito.mock(CommonRequestModel.class);
        ListCommonRequest listCommonRequest = Mockito.mock(ListCommonRequest.class);
        Mockito.when(commonRequestModel.getData()).thenReturn(listCommonRequest);

        Specification<TruckDriverDetails> specification = Mockito.mock(Specification.class);
        Pageable pageable = Mockito.mock(Pageable.class);
        Pair<Specification<TruckDriverDetails>, Pageable> tuple = Pair.of(specification, pageable);
        Mockito.when(truckDriverDetailsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(Mockito.mock(Page.class));

        // When
        ResponseEntity<IRunnerResponse> responseEntity = truckDriverDetailsService.list(commonRequestModel);

        // Then
        Mockito.verify(truckDriverDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
        // Add more assertions for the responseEntity if needed
    }
}