package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainerDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@Service
@Slf4j
public class CargoService implements ICargoService {

    private final IMDMServiceAdapter mdmServiceAdapter;
    private final ICustomerBookingDao customerBookingDao;
    private final IShipmentDao shipmentDao;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final JsonHelper jsonHelper;

    @Autowired
    public CargoService(IMDMServiceAdapter mdmServiceAdapter,
                        ICustomerBookingDao customerBookingDao,
                        IShipmentDao shipmentDao,
                        IConsolidationDetailsDao consolidationDetailsDao,
                        JsonHelper jsonHelper) {
        this.shipmentDao = shipmentDao;
        this.mdmServiceAdapter = mdmServiceAdapter;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.customerBookingDao = customerBookingDao;
        this.jsonHelper = jsonHelper;
    }

    @Override
    public ContainerDetailsResponse getContainerDetails(ContainerDetailsRequest request) throws RunnerException {
        ContainerDetailsResponse response = new ContainerDetailsResponse();
        response.setContainers(0L);
        response.setTeuCount(BigDecimal.ZERO);
        List<Containers> containers = fetchContainersByEntity(request);
        if(containers.isEmpty()){
            return response;
        }
        DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
        List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse, MdmContainerTypeResponse.class);
        Map<String, BigDecimal> codeTeuMap = containerTypes.stream()
                .collect(Collectors.toMap(
                        MdmContainerTypeResponse::getCode,
                        MdmContainerTypeResponse::getTeu
                ));
        long totalContainers = 0L;
        BigDecimal totalTeu = BigDecimal.ZERO;

        for (Containers container : containers) {
            long count = container.getContainerCount() != null ? container.getContainerCount() : 0L;
            BigDecimal teuPerContainer = codeTeuMap.getOrDefault(container.getContainerCode(), BigDecimal.ZERO);
            totalTeu = totalTeu.add(BigDecimal.valueOf(count).multiply(teuPerContainer));
            totalContainers += count;
        }

        response.setContainers(totalContainers);
        response.setTeuCount(totalTeu);
        return response;
    }

    private List<Containers> fetchContainersByEntity(ContainerDetailsRequest request) {
        String entityType = request.getEntityType();
        Long entityId = Long.valueOf(request.getEntityId());

        if (BOOKING.equals(entityType)) {
            return customerBookingDao.findById(entityId)
                    .map(CustomerBooking::getContainersList)
                    .orElse(Collections.emptyList());
        } else if (SHIPMENT.equals(entityType)) {
            return shipmentDao.findById(entityId)
                    .map(shipmentDetails -> new ArrayList<>(shipmentDetails.getContainersList()))
                    .orElseGet(ArrayList::new);
        } else if (CONSOLIDATION.equals(entityType)) {
            return consolidationDetailsDao.findById(entityId)
                    .map(ConsolidationDetails::getContainersList)
                    .orElse(Collections.emptyList());
        } else {
            log.warn("Unknown entityType '{}' in container detail request", entityType);
            return Collections.emptyList();
        }
    }
}
