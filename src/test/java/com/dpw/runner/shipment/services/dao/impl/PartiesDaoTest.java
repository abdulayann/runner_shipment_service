package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PartiesDaoTest {
    @Mock
    private IPartiesRepository partiesRepository;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IAuditLogService auditLogService;

    @InjectMocks
    private PartiesDao partiesDao;
    private static JsonTestUtility jsonTestUtility;

    private Parties testParties;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testParties = jsonTestUtility.getParties();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testSave_Success() {
        Parties parties = testParties;
        when(partiesRepository.save(parties)).thenReturn(parties);
        Parties response = partiesDao.save(parties);
        assertEquals(parties, response);
    }

    @Test
    void testSaveAll_Success() {
        Parties parties = testParties;
        when(partiesRepository.saveAll(List.of(parties))).thenReturn(List.of(parties));
        List<Parties> response = partiesDao.saveAll(List.of(parties));
        assertEquals(List.of(parties), response);
    }

    @Test
    void testFindAll_Success() {
        Page<Parties> partiesPage = mock(Page.class);
        Specification<Parties> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(partiesRepository.findAll(spec, pageable)).thenReturn(partiesPage);
        Page<Parties> parties = partiesDao.findAll(spec, pageable);
        assertEquals(partiesPage, parties);
    }

    @Test
    void testFindById_Success() {
        Optional<Parties> optionalParties = Optional.of(testParties);
        when(partiesRepository.findById(anyLong())).thenReturn(optionalParties);
        Optional<Parties> parties = partiesDao.findById(1L);
        assertTrue(parties.isPresent());
        assertEquals(testParties, parties.get());
    }

    @Test
    void testDelete_Success() {
        Parties parties = testParties;

        assertDoesNotThrow(() -> partiesDao.delete(parties));
        verify(partiesRepository, Mockito.times(1)).delete(parties);
    }

    @Test
    void testUpdateEntityFromShipment_Success() throws RunnerException {
        Parties parties = testParties;
        var spyService = Mockito.spy(partiesDao);
        doReturn(Optional.of(parties)).when(spyService).findById(anyLong());
        doReturn(parties).when(spyService).save(parties);
        Parties responseEntity = spyService.updateEntityFromShipment(parties);
        assertEquals(parties, responseEntity);
    }

    @Test
    void testUpdateEntityFromShipment_Failure(){
        Parties parties = testParties;
        var spyService = Mockito.spy(partiesDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipment(parties));
    }

    @Test
    void testUpdateEntityFromOtherEntity_Success() throws RunnerException {
        Parties parties = jsonTestUtility.getParties();
        parties.setId(2L);
        var spyService = Mockito.spy(partiesDao);
        doReturn(List.of(testParties)).when(spyService).saveEntityFromOtherEntity(any(), anyLong(), anyString(), any());
        List<Parties> responseEntity = spyService.updateEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT");
        assertEquals(List.of(testParties), responseEntity);
    }

    @Test
    void testUpdateEntityFromOtherEntity_Failure() {
        var spyService = Mockito.spy(partiesDao);
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT"));
    }

    @Test
    void testUpdateEntityFromOtherEntity_Failure_delete() throws RunnerException {
        List<Parties> partiesList = new ArrayList<>();
        partiesList.add(testParties);
        Parties parties = jsonTestUtility.getParties();
        parties.setId(2L);
        partiesList.add(parties);
        var spyService = Mockito.spy(partiesDao);
        doReturn(List.of(testParties)).when(spyService).saveEntityFromOtherEntity(any(), anyLong(), anyString(), any());
        List<Parties> responseEntity = spyService.updateEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT");
        assertEquals(List.of(testParties), responseEntity);
    }

    @Test
    void testSaveEntityFromOtherEntity_Success() {
        var spyService = Mockito.spy(partiesDao);
        doReturn(testParties).when(spyService).save(testParties);
        doReturn(Optional.of(testParties)).when(spyService).findById(anyLong());
        List<Parties> responseEntity = spyService.saveEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT");
        assertEquals(List.of(testParties), responseEntity);
    }

    @Test
    void testSaveEntityFromOtherEntity_Failure() {
        var spyService = Mockito.spy(partiesDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        List<Parties> partiesRequests = List.of(testParties);
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromOtherEntity(partiesRequests, 1L, "SHIPMENT"));
    }

    @Test
    void testSaveEntityFromOtherEntity2_Success() {
        Map<Long, Parties> oldEntityMap = new HashMap<>();
        oldEntityMap.put(testParties.getId(), testParties);
        var spyService = Mockito.spy(partiesDao);
        doReturn(List.of(testParties)).when(spyService).saveAll(List.of(testParties));
        List<Parties> responseEntity = spyService.saveEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT", oldEntityMap);
        assertEquals(List.of(testParties), responseEntity);
    }

    @Test
    void testSaveEntityFromOtherEntity2_Failure() {
        Map<Long, Parties> oldEntityMap = new HashMap<>();
        var spyService = Mockito.spy(partiesDao);
        List<Parties> partiesRequests = List.of(testParties);
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromOtherEntity(partiesRequests, 1L, "SHIPMENT", oldEntityMap));
    }

    @Test
    void testUpdateEntityFromOtherEntity2_Success() throws RunnerException {
        List<Parties> partiesList = new ArrayList<>();
        partiesList.add(testParties);
        Parties parties = jsonTestUtility.getParties();
        parties.setId(2L);
        partiesList.add(parties);
        var spyService = Mockito.spy(partiesDao);
        doReturn(List.of(testParties)).when(spyService).saveEntityFromOtherEntity(any(), anyLong(), anyString());
        List<Parties> responseEntity = spyService.updateEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT", partiesList);
        assertEquals(List.of(testParties), responseEntity);
    }

    @Test
    void testUpdateEntityFromOtherEntity2_Failure() {
        List<Parties> partiesList = new ArrayList<>();
        partiesList.add(testParties);
        Parties parties = jsonTestUtility.getParties();
        parties.setId(2L);
        partiesList.add(parties);
        var spyService = Mockito.spy(partiesDao);
        doThrow(new RuntimeException()).when(spyService).saveEntityFromOtherEntity(any(), anyLong(), anyString());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromOtherEntity(List.of(testParties), 1L, "SHIPMENT", partiesList));
    }

}
