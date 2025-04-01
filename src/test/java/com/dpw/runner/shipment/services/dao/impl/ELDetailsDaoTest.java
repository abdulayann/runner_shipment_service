package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IELDetailsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ELDetailsDaoTest {

    @InjectMocks
    private ELDetailsDao dao;
    @Mock
    private IELDetailsRepository elDetailsRepository;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IAuditLogService auditLogService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        ELDetails elDetails = ELDetails.builder().build();
        when(elDetailsRepository.save(any())).thenReturn(elDetails);
        assertEquals(elDetails, dao.save(elDetails));
    }

    @Test
    void saveAll() {
        ELDetails testData = ELDetails.builder().build();
        when(elDetailsRepository.saveAll(any())).thenReturn(List.of(testData));
        assertEquals(List.of(testData), dao.saveAll(List.of(testData)));
    }

    @Test
    void findByGuid() {
        ELDetails testData = ELDetails.builder().build();
        testData.setGuid(UUID.randomUUID());

        when(elDetailsRepository.findByGuid(any())).thenReturn(Optional.of(testData));
        assertEquals(testData, dao.findByGuid(UUID.randomUUID()).get());
    }

    @Test
    void findAll() {
        ELDetails testData = ELDetails.builder().build();
        testData.setShipmentId(1L);

        List<ELDetails> elDetailsList = new ArrayList<>();
        elDetailsList.add(testData);

        PageImpl<ELDetails> elDetailsPage = new PageImpl<>(elDetailsList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<ELDetails>, Pageable> pair = fetchData(listReq, HblTermsConditionTemplate.class);

        when(elDetailsRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(elDetailsPage);
        assertEquals(elDetailsPage, dao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findById() {
        ELDetails testData = ELDetails.builder().build();
        when(elDetailsRepository.findById(any())).thenReturn(Optional.of(testData));
        assertEquals(testData, dao.findById(1L).get());
    }

    @Test
    void delete() {
        ELDetails testData = ELDetails.builder().build();
        dao.delete(testData);
        verify(elDetailsRepository, times(1)).delete(testData);
    }

    @Test
    void findByElNumber() {
        ELDetails testData = ELDetails.builder().build();
        when(elDetailsRepository.findByElNumber(anyString())).thenReturn(Optional.of(testData));
        assertEquals(testData, dao.findByElNumber("El123").get());
    }

    @Test
    void updateEntityFromShipment() throws RunnerException {
        ELDetails testData = ELDetails.builder().build();
        testData.setId(1L);
        testData.setShipmentId(5L);
        List<ELDetails> elDetailsList = Arrays.asList(testData);
        when(elDetailsRepository.findByShipmentId(anyLong())).thenReturn(elDetailsList);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(elDetailsRepository.saveAll(any())).thenReturn(elDetailsList);
        assertEquals(elDetailsList, dao.updateEntityFromShipment(elDetailsList, 5L));
    }

    @Test
    void saveEntityFromShipmentEntityNotPresent() {
        ELDetails testData = ELDetails.builder().build();
        testData.setId(1L);
        testData.setShipmentId(5L);
        List<ELDetails> elDetailsList = Arrays.asList(testData);

        when(elDetailsRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> {
            dao.saveEntityFromShipment(elDetailsList, 5L);
        });
    }

    @Test
    void saveEntityFromShipment() {
        ELDetails testData = ELDetails.builder().build();
        testData.setId(1L);
        testData.setShipmentId(5L);
        List<ELDetails> elDetailsList = Arrays.asList(testData);

        when(elDetailsRepository.findById(any())).thenReturn(Optional.of(testData));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(elDetailsRepository.save(any())).thenReturn(testData);

        assertEquals(elDetailsList, dao.saveEntityFromShipment(elDetailsList, 5L));
    }

    @Test
    void updateEntityFromShipmentWithOldEntity() throws RunnerException {
        ELDetails testData = ELDetails.builder().build();
        testData.setId(1L);
        testData.setShipmentId(5L);
        testData.setGuid(UUID.randomUUID());
        List<ELDetails> elDetailsList = Arrays.asList(testData);

        when(elDetailsRepository.findById(any())).thenReturn(Optional.of(testData));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(elDetailsRepository.save(any())).thenReturn(testData);

        assertEquals(elDetailsList, dao.updateEntityFromShipment(elDetailsList, 5L, elDetailsList));
    }

    @Test
    void updateEntityFromShipmentWithOldEntityElDetails() throws RunnerException {
        ELDetails testData = ELDetails.builder().build();
        testData.setId(1L);
        testData.setShipmentId(5L);
        testData.setGuid(UUID.randomUUID());
        List<ELDetails> elDetailsList = Arrays.asList(testData);

        when(jsonHelper.convertToJson(any())).thenReturn("");
        doNothing().when(elDetailsRepository).delete(any());

        assertEquals(new ArrayList<>(), dao.updateEntityFromShipment(new ArrayList<>(), 5L, elDetailsList));
    }

    @Test
    void updateEntityFromShipmentCatch() throws RunnerException {
        when(dao.findByShipmentId(anyLong())).thenThrow(new RuntimeException());
        assertThrows(RunnerException.class, () -> {
            dao.updateEntityFromShipment(new ArrayList<>(), -1L);
        });
    }

    @Test
    void updateEntityFromShipmentOldEntityDeleteELDetailCatch() throws RunnerException {
        ELDetails testData = ELDetails.builder().build();
        testData.setId(1L);
        testData.setShipmentId(5L);
        testData.setGuid(UUID.randomUUID());
        List<ELDetails> elDetailsList = Arrays.asList(testData);

        when(jsonHelper.convertToJson(any())).thenThrow(new RuntimeException());
        assertEquals(new ArrayList<>(), dao.updateEntityFromShipment(new ArrayList<>(), 5L, elDetailsList));
    }
}