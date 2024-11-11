package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dto.request.ReassignRequest;
import com.dpw.runner.shipment.services.dto.request.RequestForTransferRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;

import java.util.Optional;
import java.util.concurrent.ExecutorService;
import static org.mockito.Mockito.*;

import static org.junit.jupiter.api.Assertions.*;
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NetworkTransferServiceTest {

    @Mock
    private ModelMapper modelMapper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private INetworkTransferDao networkTransferDao;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ExecutorService executorService;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @InjectMocks
    private NetworkTransferService networkTransferService;
    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build());
    }

    @Test
    void requestForTransfer() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().build()));
        var response = networkTransferService.requestForTransfer(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForTransfer_DataRetrievalFailure() {
        RequestForTransferRequest requestForTransferRequest = RequestForTransferRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(requestForTransferRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForTransfer(request));
    }

    @Test
    void requestForReassign() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.of(NetworkTransfer.builder().build()));
        var response = networkTransferService.requestForReassign(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void requestForReassign_DataRetrievalFailure() {
        ReassignRequest reassignRequest = ReassignRequest.builder().id(12L).remarks("Test").build();
        var request = CommonRequestModel.builder().data(reassignRequest).build();
        when(networkTransferDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> networkTransferService.requestForReassign(request));
    }
}