package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IServiceDetailsDao;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

@Repository
@Slf4j
public class ServiceDetailsDao implements IServiceDetailsDao {
    @Autowired
    private IServiceDetailsRepository serviceDetailsRepository;

    @Override
    public ServiceDetails save(ServiceDetails serviceDetails) {
        return serviceDetailsRepository.save(serviceDetails);
    }

    @Override
    public Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable) {
        return serviceDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ServiceDetails> findById(Long id) {
        return serviceDetailsRepository.findById(id);
    }

    @Override
    public void delete(ServiceDetails serviceDetails) {
        serviceDetailsRepository.delete(serviceDetails);
    }

    public List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<ServiceDetails> responseServiceDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
            Page<ServiceDetails> serviceDetailsPage = findAll(pair.getLeft(), pair.getRight());
            Map<Long, ServiceDetails> hashMap = serviceDetailsPage.stream()
                    .collect(Collectors.toMap(ServiceDetails::getId, Function.identity()));
            List<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
            if (serviceDetailsList != null && serviceDetailsList.size() != 0) {
                for (ServiceDetails request : serviceDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    serviceDetailsRequests.add(request);
                }
                responseServiceDetails = saveServiceDetails(serviceDetailsRequests, shipmentId);
            }
            deleteServiceDetails(hashMap);
            return responseServiceDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<ServiceDetails> saveServiceDetails(List<ServiceDetails> serviceDetailsRequests, Long shipmentId) {
        List<ServiceDetails> res = new ArrayList<>();
        for(ServiceDetails req : serviceDetailsRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<ServiceDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("ServiceDetails is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteServiceDetails(Map<Long, ServiceDetails> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }
}
