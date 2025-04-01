package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.repository.interfaces.ITenantProductsRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class TenantProductsDao implements ITenantProductsDao {

    @Autowired
    private ITenantProductsRepository tenantProductsRepository;

    @Override
    public TenantProducts save(TenantProducts tenantProducts) {
        return tenantProductsRepository.save(tenantProducts);
    }

    public Optional<TenantProducts> findById(Long id) {
        return tenantProductsRepository.findById(id);
    }

    @Override
    public List<TenantProducts> saveAll(List<TenantProducts> tenantProductsList) {
        return tenantProductsRepository.saveAll(tenantProductsList);
    }

    @Override
    public Page<TenantProducts> findAll(Specification<TenantProducts> spec, Pageable pageable) {
        return tenantProductsRepository.findAll(spec, pageable);
    }

    @Override
    public List<TenantProducts> saveEntityFromSettings(List<TenantProducts> tenantProductsList, Long shipmentSettingsId) {
        List<TenantProducts> res = new ArrayList<>();
        for (TenantProducts req : tenantProductsList) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<TenantProducts> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Tenant Product is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentSettingsId(shipmentSettingsId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    @Override
    public List<TenantProducts> updateEntityFromSettings(List<TenantProducts> tenantProductsList, Long shipmentSettingsId) throws RunnerException {
        String responseMsg;
        List<TenantProducts> responseTenantProducts = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsId, "=");
            Pair<Specification<TenantProducts>, Pageable> pair = fetchData(listCommonRequest, TenantProducts.class);
            Page<TenantProducts> tenantProducts = findAll(pair.getLeft(), pair.getRight());
            Map<Long, TenantProducts> hashMap = tenantProducts.stream()
                    .collect(Collectors.toMap(TenantProducts::getId, Function.identity()));
            List<TenantProducts> tenantProductsRequestList = new ArrayList<>();
            if (tenantProductsList != null && tenantProductsList.size() != 0) {
                for (TenantProducts request : tenantProductsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    tenantProductsRequestList.add(request);
                }
                responseTenantProducts = saveEntityFromSettings(tenantProductsRequestList, shipmentSettingsId);
            }
            deleteTenantProducts(hashMap);
            return responseTenantProducts;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<TenantProducts> updateEntityFromV1Settings(List<TenantProducts> tenantProductsList, Long shipmentSettingsId, List<TenantProducts> oldTenantProducts) throws RunnerException {
        String responseMsg;
        List<TenantProducts> responseTenantProducts = new ArrayList<>();
        try {
            Map<UUID, TenantProducts> hashMap = new HashMap<>();
            if(oldTenantProducts != null && oldTenantProducts.size() > 0)
                hashMap = oldTenantProducts.stream().collect(Collectors.toMap(TenantProducts::getGuid, Function.identity()));
            List<TenantProducts> tenantProductsRequestList = new ArrayList<>();
            if (tenantProductsList != null && tenantProductsList.size() != 0) {
                for (TenantProducts request : tenantProductsList) {
                    UUID guid = request.getGuid();
                    if(hashMap.containsKey(guid)) {
                        request.setId(hashMap.get(guid).getId());
                        hashMap.remove(guid);
                    }
                    tenantProductsRequestList.add(request);
                }
                responseTenantProducts = saveEntityFromSettings(tenantProductsRequestList, shipmentSettingsId);
            }
            deleteTenantProductsByUUID(hashMap);
            return responseTenantProducts;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public void delete(TenantProducts tenantProducts) {
        tenantProductsRepository.delete(tenantProducts);
    }

    private void deleteTenantProducts(Map<Long, TenantProducts> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    private void deleteTenantProductsByUUID(Map<UUID, TenantProducts> hashMap) {
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
