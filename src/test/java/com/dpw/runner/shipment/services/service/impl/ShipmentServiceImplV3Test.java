package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.mapper.ShipmentMapper;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShipmentMasterDataHelperV3;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentServiceImplV3Test {

    @InjectMocks
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private INotificationDao notificationDao;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private IShipmentRepository shipmentRepository;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private ShipmentMasterDataHelperV3 shipmentMasterDataHelper;


    @BeforeAll
    static void init() throws IOException {
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
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
    }

    @Test
    void testGetPendingNotificationCount() {

        when(consoleShipmentMappingDao.pendingStateCountBasedOnRequestType(any(), any())).thenReturn(1);
        when(notificationDao.findAllPendingNotificationCount(any(), any())).thenReturn(1);

        var response = shipmentServiceImplV3.getPendingNotificationCount();
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void listRequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listNullIncludeColumns() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(new ArrayList<>()).build();
        listCommonRequest.setNotificationFlag(true);
        listCommonRequest.setIncludeColumns(List.of());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listShipmentsWithNotifications() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(new ArrayList<>()).build();
        listCommonRequest.setNotificationFlag(true);
        listCommonRequest.setIncludeColumns(List.of("ordersCount", "pickupDetails.shipperRef"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentListResponse shipmentListResponse = new ShipmentListResponse();
        shipmentListResponse.setContainsHazardous(false);
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        PageImpl<Long> shipmentIdPage = new PageImpl<>(List.of(1L));
        when(shipmentRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(commonUtils.setIncludedFieldsToResponse(any(), anySet(), any())).thenReturn(shipmentListResponse);
        when(shipmentDao.getIdWithPendingActions(eq(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED), any())).thenReturn(shipmentIdPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentServiceImplV3.listShipment(commonRequestModel, true);
        assertEquals(expectedResponse, httpResponse);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ShipmentListResponse> shipmentListResponses  = ShipmentMapper.INSTANCE.toShipmentListResponses(lst);
        for(var i: shipmentListResponses) {
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                i.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            if (ObjectUtils.isNotEmpty(i.getShipmentOrders()))
                i.setOrdersCount(i.getShipmentOrders().size());
            responseList.add(i);
        }

        return responseList;
    }

}