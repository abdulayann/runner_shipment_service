package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiReferenceDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsReferenceRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionLegsReferenceServiceImpl.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class TransportInstructionLegsReferenceServiceImplTest {
    @MockBean
    private DependentServiceHelper dependentServiceHelper;

    @MockBean
    private IAuditLogService iAuditLogService;

    @MockBean
    private ITiReferenceDao iTiReferenceDao;

    @MockBean
    private ITiLegRepository iTiLegRepository;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private UserContext userContext;

    @Autowired
    private TransportInstructionLegsReferenceServiceImpl transportInstructionLegsReferenceService;

    @Test
    void testCreate() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsReferenceRequest request = new TransportInstructionLegsReferenceRequest();
        request.setReference("ref123");
        request.setType("BLE");
        request.setTiLegId(1l);
        TiReferences tiReferences = new TiReferences();
        tiReferences.setTiLegId(1l);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(1l);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsReferenceResponse.class))).thenReturn(new TransportInstructionLegsReferenceResponse());
        when(jsonHelper.convertValue(request, TiReferences.class)).thenReturn(new TiReferences());
        when(iTiReferenceDao.save(any())).thenReturn(new TiReferences());
        when(iTiLegRepository.findById(anyLong())).thenReturn(Optional.of(tiLegs));
        TransportInstructionLegsReferenceResponse response = transportInstructionLegsReferenceService.create(request);
        assertNotNull(response);
    }

    @Test
    void testUpdate() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        TransportInstructionLegsReferenceRequest request = new TransportInstructionLegsReferenceRequest();
        request.setReference("ref123");
        request.setType("BLE");
        request.setTiLegId(1l);
        request.setId(1l);
        TiReferences tiReferences = new TiReferences();
        tiReferences.setTiLegId(1l);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(1l);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsReferenceResponse.class))).thenReturn(new TransportInstructionLegsReferenceResponse());
        when(jsonHelper.convertValue(request, TiReferences.class)).thenReturn(new TiReferences());
        when(iTiReferenceDao.findById(anyLong())).thenReturn(Optional.of(new TiReferences()));
        when(iTiReferenceDao.save(any())).thenReturn(new TiReferences());
        when(iTiLegRepository.findById(anyLong())).thenReturn(Optional.of(tiLegs));
        TransportInstructionLegsReferenceResponse response = transportInstructionLegsReferenceService.update(request);
        assertNotNull(response);
    }

    @Test
    void testDelete3() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(1l);
        when(iTiLegRepository.findById(any())).thenReturn(Optional.of(tiLegs));
        when(iTiReferenceDao.findById(Mockito.<Long>any())).thenReturn(Optional.of(new TiReferences()));
        doNothing().when(iTiReferenceDao).delete(any());
        transportInstructionLegsReferenceService.delete(1L);
        verify(iTiReferenceDao).findById(Mockito.<Long>any());
    }


    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#retrieveById(Long)}
     */
    @Test
    void testRetrieveById() {
        TransportInstructionLegsReferenceResponse response = TransportInstructionLegsReferenceResponse
                .builder()
                .id(1l)
                .tiLegId(1l)
                .reference("ref123")
                .type("BLE").build();


        when(jsonHelper.convertValue(Mockito.<TiReferences>any(),
                Mockito.<Class<TransportInstructionLegsReferenceResponse>>any())).thenReturn(response);

        TiReferences tiReferences = new TiReferences();
        Optional<TiReferences> ofResult = Optional.of(tiReferences);
        when(iTiReferenceDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        transportInstructionLegsReferenceService.retrieveById(1L);
        verify(iTiReferenceDao).findById(Mockito.<Long>any());
        verify(jsonHelper).convertValue(Mockito.<TiTruckDriverDetails>any(),
                Mockito.<Class<TransportInstructionLegsTruckDriverResponse>>any());
    }

    @Test
    void testList2() {
        TransportInstructionLegsReferenceResponse response = TransportInstructionLegsReferenceResponse
                .builder()
                .id(1l)
                .tiLegId(1l)
                .reference("ref123")
                .type("BLE").build();

        when(jsonHelper.convertValue(Mockito.<TiReferences>any(),
                Mockito.<Class<TransportInstructionLegsReferenceResponse>>any())).thenReturn(response);
        TiReferences tiReferences = new TiReferences();
        tiReferences.setTiLegId(1l);
        tiReferences.setReference("ref123");
        tiReferences.setType("BLE");

        ArrayList<TiReferences> content = new ArrayList<>();
        content.add(tiReferences);
        PageImpl<TiReferences> pageImpl = new PageImpl<>(content);
        when(iTiReferenceDao.findAll(Mockito.<Specification<TiReferences>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        TransportInstructionLegsReferenceListResponse actualListResult = transportInstructionLegsReferenceService
                .list(new ListCommonRequest());
        verify(iTiReferenceDao).findAll(Mockito.<Specification<TiReferences>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiReferences>any(),
                Mockito.<Class<TransportInstructionLegsReferenceResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(1, actualListResult.getTiLegsReferenceResponses().size());
        assertEquals(1L, actualListResult.getTotalCount().longValue());
    }
}
