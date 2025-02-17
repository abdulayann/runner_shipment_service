package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
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

@Repository
@Slf4j
public class NotesDao implements INotesDao {
    @Autowired
    private INotesRepository notesRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private ValidatorUtility validatorUtility;

    @Override
    public Notes save(Notes notes) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(notes), Constants.NOTES, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return notesRepository.save(notes);
    }

    @Override
    public List<Notes> saveAll(List<Notes> notesList) {
        for (var notes : notesList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(notes), Constants.NOTES, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return notesRepository.saveAll(notesList);
    }

    @Override
    public Page<Notes> findAll(Specification<Notes> spec, Pageable pageable) {
        return notesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Notes> findById(Long id) {
        return notesRepository.findById(id);
    }

    @Override
    public void delete(Notes notes) {
        notesRepository.delete(notes);
    }

    @Override
    public List<Notes> findByEntityIdAndEntityType(Long entityId, String entityType) {
        return notesRepository.findByEntityIdAndEntityType(entityId, entityType);
    }

    public List<Notes> updateEntityFromOtherEntity(List<Notes> notesList, Long entityId, String entityType) throws RunnerException {
        String responseMsg;
        List<Notes> responseNotes = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            List<Notes> notes = findByEntityIdAndEntityType(entityId, entityType);
            Map<Long, Notes> hashMap = notes.stream()
                    .collect(Collectors.toMap(Notes::getId, Function.identity()));
            Map<Long, Notes> copyHashMap = new HashMap<>(hashMap);
            List<Notes> notesRequestList = new ArrayList<>();
            if (notesList != null && notesList.size() != 0) {
                for (Notes request : notesList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    notesRequestList.add(request);
                }
                responseNotes = saveEntityFromOtherEntity(notesRequestList, entityId, entityType, copyHashMap);
            }
            deleteNotes(hashMap, entityType, entityId);
            return responseNotes;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<Notes> saveEntityFromOtherEntity(List<Notes> notesRequests, Long entityId, String entityType) {
        List<Notes> res = new ArrayList<>();
        for (Notes req : notesRequests) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Notes> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Notes is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Notes.class) : null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
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

    @Override
    public List<Notes> saveEntityFromOtherEntity(List<Notes> notesRequests, Long entityId, String entityType, Map<Long, Notes> oldEntityMap) {
        List<Notes> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (Notes req : notesRequests) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Notes is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (oldEntityJsonStringMap.containsKey(req.getId())) {
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Notes.class) : null)
                                .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
                                .parentId(entityId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    private void deleteNotes(Map<Long, Notes> hashMap, String entityType, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(note -> {
                String json = jsonHelper.convertToJson(note);
                delete(note);
                if (entityType != null) {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Notes.class))
                                        .parent(Objects.equals(entityType, Constants.SHIPMENT) ? ShipmentDetails.class.getSimpleName() : entityType)
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

    public List<Notes> updateEntityFromOtherEntity(List<Notes> notesList, Long entityId, String entityType, List<Notes> oldEntityList) throws RunnerException {
        String responseMsg;
        List<Notes> responseNotes = new ArrayList<>();
        Map<UUID, Notes> notesMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (Notes entity :
                    oldEntityList) {
                notesMap.put(entity.getGuid(), entity);
            }
        }
        try {
            Notes oldEntity;
            List<Notes> notesRequestList = new ArrayList<>();
            if (notesList != null && notesList.size() != 0) {
                for (Notes request : notesList) {
                    oldEntity = notesMap.get(request.getGuid());
                    if (oldEntity != null) {
                        notesMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    request.setEntityId(entityId);
                    request.setEntityType(entityType);
                    notesRequestList.add(request);
                }
                responseNotes = saveEntityFromOtherEntity(notesRequestList, entityId, entityType);
            }
            Map<Long, Notes> hashMap = new HashMap<>();
            notesMap.forEach((s, notes) -> hashMap.put(notes.getId(), notes));
            deleteNotes(hashMap, entityType, entityId);
            return responseNotes;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }
}
