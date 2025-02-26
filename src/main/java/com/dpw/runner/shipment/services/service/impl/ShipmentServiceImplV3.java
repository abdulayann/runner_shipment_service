package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.INotificationDao;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentServiceImplV3 implements IShipmentServiceV3 {

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private INotificationDao notificationDao;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotificationCount() {

        Integer tenantId = TenantContext.getCurrentTenant();
        Integer count = consoleShipmentMappingDao.pendingStateCountBasedOnRequestType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED.ordinal(), tenantId);
        count += notificationDao.findAllPendingNotificationCount(SHIPMENT, tenantId);

        return ResponseHelper.buildSuccessResponse(NotificationCount.builder().count(count).build());
    }

    @Override
    public ResponseEntity<IRunnerResponse> listShipment(ListCommonRequest listCommonRequest, boolean getMasterData) {
        String responseMsg;
        try {
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listCommonRequest, ShipmentDetails.class, ShipmentConstants.TABLES_NAMES);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentRepository.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_V3_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
                throw new ValidationException("Include Columns field is mandatory");
            }
            List<IRunnerResponse> filteredList = new ArrayList<>();

            for (var curr : shipmentDetailsPage.getContent()) {
                ShipmentDetailsResponse shipmentDetailsResponse = commonUtils.getShipmentDetailsV3Response(curr, listCommonRequest.getIncludeColumns());
                filteredList.add(shipmentDetailsResponse);
            }
            return ResponseHelper.buildListSuccessResponse(
                    filteredList,
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
