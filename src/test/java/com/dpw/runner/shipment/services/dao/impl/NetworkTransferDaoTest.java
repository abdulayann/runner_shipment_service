package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferRepository;
import com.dpw.runner.shipment.services.service.impl.AuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NetworkTransferDaoTest {
    @Mock
    private INetworkTransferRepository networkTransferRepository;
    @Mock
    private ValidatorUtility validatorUtility;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private AuditLogService auditLogService;
    @InjectMocks
    private NetworkTransferDao networkTransferDao;

    private static JsonTestUtility jsonTestUtility;
    private static NetworkTransfer networkTransfer;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }


    @BeforeEach
    void setUp() {
        networkTransfer = jsonTestUtility.getNetworkTransfer();
        UserContext.setUser(UsersDto.builder().Username("user").build());
    }

    @Test
    void updateStatusAndCreatedEntityId(){
        networkTransferDao.updateStatusAndCreatedEntityId(1L, NetworkTransferStatus.ACCEPTED.name(), 21L);
        verify(networkTransferRepository, times(1)).updateStatusAndCreatedEntityId(1L, NetworkTransferStatus.ACCEPTED.name(), 21L);
    }

    @Test
    void testFindByTenantAndEntity(){
        networkTransferDao.findByTenantAndEntity(1,21L, Constants.SHIPMENT);
        verify(networkTransferRepository, times(1)).findByTenantAndEntity(1,21L, Constants.SHIPMENT);
    }

    @Test
    void testDelete(){
        networkTransferDao.delete(any());
        verify(networkTransferRepository, times(1)).delete(any());
    }

    @Test
    void testDeleteAndLog(){
        networkTransferDao.deleteAndLog(networkTransfer, Constants.SHIPMENT);
        verify(networkTransferRepository, times(1)).delete(any());
    }

    @Test
    void testFindByGuid(){
        UUID randomGuid = UUID.fromString("f2fbf3e9-3f37-439b-a45e-d4f7d08885c9");
        Mockito.when(networkTransferRepository.findByGuid(randomGuid)).thenReturn(Optional.of(networkTransfer));
        Optional<NetworkTransfer> networkTransfer1 = networkTransferDao.findByGuid(randomGuid);
        verify(networkTransferRepository, times(1)).findByGuid(randomGuid);
        assert(Objects.equals(randomGuid, networkTransfer1.get().getGuid()));
    }

    @Test
    void testFindById(){
        Long networkTransferId = 21L;
        Mockito.when(networkTransferRepository.findById(networkTransferId)).thenReturn(Optional.of(networkTransfer));
        Optional<NetworkTransfer> networkTransfer1 = networkTransferDao.findById(networkTransferId);
        verify(networkTransferRepository, times(1)).findById(networkTransferId);
        assert(Objects.equals(networkTransferId, networkTransfer1.get().getId()));
    }

    @Test
    void testFindAll(){
        Specification<NetworkTransfer> spec = null;
        Pageable pageable = null;
        List<NetworkTransfer> networkTransferList = new ArrayList<>();
        Page<NetworkTransfer> networkTransfersList = new PageImpl<>(networkTransferList);
        Mockito.when(networkTransferRepository.findAll(spec, pageable)).thenReturn(networkTransfersList);
        Page<NetworkTransfer> networkTransfers = networkTransferDao.findAll(spec, pageable);
        assertEquals(networkTransfersList.getTotalElements(), networkTransfers.getTotalElements());
    }

    @Test
    void testSaveSuccess() {
        NetworkTransfer networkTransfer1 = new NetworkTransfer();
        Mockito.when(networkTransferRepository.save(Mockito.any())).thenReturn(networkTransfer1);
        NetworkTransfer notes1 = networkTransferDao.save(networkTransfer1);
        assertEquals(networkTransfer1, notes1);
    }

    @Test
    void testSaveWithEntitySuccess() {
        NetworkTransfer newNetworkTransfer = new NetworkTransfer();
        newNetworkTransfer.setId(21L);
        Mockito.when(networkTransferRepository.findById(21L)).thenReturn(Optional.of(newNetworkTransfer));
        Mockito.when(networkTransferRepository.save(Mockito.any())).thenReturn(newNetworkTransfer);
        NetworkTransfer networkTransfer1 = networkTransferDao.save(newNetworkTransfer);
        assertEquals(newNetworkTransfer, networkTransfer1);
        verify(networkTransferRepository, times(1)).findById(21L);
        verify(networkTransferRepository, times(1)).save(any());
    }

    @Test
    void testSaveWithEntityFailure() {
        HashSet<String> error = new HashSet<>();

        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().build();
        networkTransfer1.setId(1L);

        when(networkTransferRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(DataRetrievalFailureException.class, () -> {
            networkTransferDao.save(networkTransfer1);
        });
    }

    @Test
    void SaveError() {
        HashSet<String> error = new HashSet<>();
        error.add("An Error Occurred");
        NetworkTransfer networkTransfer1 = NetworkTransfer.builder().build();
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            networkTransferDao.save(networkTransfer1);
        });
    }

    @Test
    void findByEntityIdAndEntityTypeAndIsInterBranchEntityTest(){
        networkTransferDao.findByEntityIdAndEntityTypeAndIsInterBranchEntity(anyList(), anyString(), anyBoolean(), anyList(), any());
        verify(networkTransferRepository, times(1)).findByEntityIdAndEntityTypeAndIsInterBranchEntity(anyList(), anyString(), anyBoolean(), anyList(), any());
    }

    @Test
    void getInterConsoleNTListTest(){
        networkTransferDao.getInterConsoleNTList(anyList(), anyString());
        verify(networkTransferRepository, times(1)).getInterConsoleNTList(anyList(), anyString());
    }

    @Test
    void deleteByIdsAndLog(){
        networkTransferDao.deleteByIdsAndLog(anyList());
        verify(networkTransferRepository, times(1)).deleteAllById(anyList());
    }

    @Test
    void findStatusByEntityIdAndEntityTypeAndTenantId() {
        when(networkTransferRepository.findStatusByEntityIdAndEntityTypeAndTenantId(anyLong(), anyString(), any())).thenReturn(NetworkTransferStatus.SCHEDULED.name());
        var res = networkTransferDao.findStatusByEntityIdAndEntityTypeAndTenantId(anyLong(), anyString(), any());
        assertEquals(NetworkTransferStatus.SCHEDULED.name(), res);
    }

    @Test
    void findByEntityGuidAndTenantId() {
        when(networkTransferRepository.findByEntityGuidAndTenantId(any(), any())).thenReturn(NetworkTransferStatus.SCHEDULED.name());
        var res = networkTransferDao.findByEntityGuidAndTenantId(any(), any());
        assertEquals(NetworkTransferStatus.SCHEDULED.name(), res);
    }

    @Test
    void testFindNteForMigrationStatuses() {
        List<String> statuses = List.of("MIGRATED", "PENDING");
        Integer tenantId = 1;
        List<Long> expected = List.of(10L, 20L);

        when(networkTransferRepository.findNteForMigrationStatuses(statuses, tenantId)).thenReturn(expected);

        List<Long> actual = networkTransferDao.findNteForMigrationStatuses(statuses, tenantId);

        assertEquals(expected, actual);
        verify(networkTransferRepository).findNteForMigrationStatuses(statuses, tenantId);
    }

    @Test
    void testFindNteByIds() {
        List<Long> ids = List.of(100L, 200L);
        NetworkTransfer nt1 = new NetworkTransfer();
        nt1.setId(100L);
        NetworkTransfer nt2 = new NetworkTransfer();
        nt2.setId(200L);
        List<NetworkTransfer> expected = List.of(nt1, nt2);
        when(networkTransferRepository.findNteByIds(ids)).thenReturn(expected);
        List<NetworkTransfer> actual = networkTransferDao.findNteByIds(ids);
        assertEquals(expected, actual);
        verify(networkTransferRepository).findNteByIds(ids);
    }

    @Test
    void testFindByEntityIdsAndEntityType() {
        Set<Long> entityIds = Set.of(1L, 2L);
        String entityType = "SOME_TYPE";
        NetworkTransfer nt1 = new NetworkTransfer();
        nt1.setId(1L);
        NetworkTransfer nt2 = new NetworkTransfer();
        nt2.setId(2L);
        List<NetworkTransfer> expected = List.of(nt1, nt2);
        when(networkTransferRepository.findByEntityIdAndEntityType(entityIds, entityType)).thenReturn(expected);
        List<NetworkTransfer> actual = networkTransferDao.findByEntityIdsAndEntityType(entityIds, entityType);
        assertEquals(expected, actual);
        verify(networkTransferRepository).findByEntityIdAndEntityType(entityIds, entityType);
    }

    @Test
    void testFindNteForMigrationStatuses_returnsMatchingIds() {
        List<String> statuses = List.of("MIGRATED", "PENDING");
        Integer tenantId = 101;
        List<Long> expectedIds = List.of(100L, 200L);
        when(networkTransferRepository.findNteForMigrationStatuses(statuses, tenantId))
                .thenReturn(expectedIds);
        List<Long> actual = networkTransferDao.findNteForMigrationStatuses(statuses, tenantId);
        assertEquals(expectedIds, actual);
        verify(networkTransferRepository).findNteForMigrationStatuses(statuses, tenantId);
    }

    @Test
    void testFindByEntityIdsAndEntityType_returnsEntities() {
        Set<Long> entityIds = Set.of(10L, 20L);
        String entityType = "SHIPMENT";
        NetworkTransfer nt1 = new NetworkTransfer();
        nt1.setId(10L);
        NetworkTransfer nt2 = new NetworkTransfer();
        nt2.setId(20L);
        List<NetworkTransfer> expected = List.of(nt1, nt2);
        when(networkTransferRepository.findByEntityIdAndEntityType(entityIds, entityType))
                .thenReturn(expected);
        List<NetworkTransfer> actual = networkTransferDao.findByEntityIdsAndEntityType(entityIds, entityType);
        assertEquals(expected, actual);
        verify(networkTransferRepository).findByEntityIdAndEntityType(entityIds, entityType);
    }
}