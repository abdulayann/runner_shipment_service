package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferRepository;
import com.dpw.runner.shipment.services.service.impl.AuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

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

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build());
    }

    @Test
    void updateStatusAndCreatedEntityId(){
        networkTransferDao.updateStatusAndCreatedEntityId(1L, NetworkTransferStatus.ACCEPTED, 21L);
        verify(networkTransferRepository, times(1)).updateStatusAndCreatedEntityId(1L, NetworkTransferStatus.ACCEPTED, 21L);
    }

}