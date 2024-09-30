package com.dpw.runner.booking.services.dao.impl;

import com.dpw.runner.booking.services.commons.constants.Constants;
import com.dpw.runner.booking.services.commons.constants.DaoConstants;
import com.dpw.runner.booking.services.commons.enums.DBOperationType;
import com.dpw.runner.booking.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.booking.services.commons.requests.ListCommonRequest;
import com.dpw.runner.booking.services.dao.interfaces.IPackingDao;
import com.dpw.runner.booking.services.entity.CustomerBooking;
import com.dpw.runner.booking.services.entity.Packing;
import com.dpw.runner.booking.services.entity.enums.LifecycleHooks;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import com.dpw.runner.booking.services.exception.exceptions.ValidationException;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.booking.services.service.interfaces.IAuditLogService;
import com.dpw.runner.booking.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.booking.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.booking.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class PackingDao implements IPackingDao {
    public static final String PACKING_IS_NULL_FOR_ID_MSG = "Packing is null for Id {}";
    @Autowired
    private IPackingRepository packingRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public Packing save(Packing packing) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(packing), Constants.PACKING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return packingRepository.save(packing);
    }

    @Override
    public Page<Packing> findAll(Specification<Packing> spec, Pageable pageable) {
        return packingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Packing> findById(Long id) {
        return packingRepository.findById(id);
    }

    @Override
    public Optional<Packing> findByGuid(UUID id) {
        return packingRepository.findByGuid(id);
    }

    @Override
    public void delete(Packing packing) {
        packingRepository.delete(packing);
    }

    public List<Packing> updateEntityFromBooking(List<Packing> packingList, Long bookingId) throws RunnerException {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Packing> hashMap = packings.stream()
                    .collect(Collectors.toMap(Packing::getId, Function.identity()));
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromBooking(packingRequestList, bookingId);
            }
            deletePackings(hashMap, "CustomerBooking", bookingId);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Packing> getAllPackings() {
        return packingRepository.findAll();
    }

    @Override
    public List<Packing> saveAll(List<Packing> packingList) {
        for(var packing : packingList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(packing), Constants.PACKING, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return packingRepository.saveAll(packingList);
    }

    public List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookingId) {
        List<Packing> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packingPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Packing> hashMap = packingPage.stream()
                .collect(Collectors.toMap(Packing::getId, Function.identity()));
        for (Packing req : packings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug(PACKING_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                operation = DBOperationType.UPDATE.name();
            }
            req.setBookingId(bookingId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Packing.class) : null)
                                .parent(CustomerBooking.class.getSimpleName())
                                .parentId(bookingId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }

    private void deletePackings(Map<Long, Packing> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(packing -> {
                String json = jsonHelper.convertToJson(packing);
                delete(packing);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Packing.class))
                                        .parent(entity)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                             InvocationTargetException | NoSuchMethodException | RunnerException e) {
                        log.error(e.getMessage());
                    }
                }
            });
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<Packing> findByConsolidationId(Long consolidationId) {
        return packingRepository.findByConsolidationId(consolidationId);
    }
}
