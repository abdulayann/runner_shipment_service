package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;

@Repository
@Slf4j
public class NotesDao implements INotesDao {
    @Autowired
    private INotesRepository notesRepository;

    @Override
    public Notes save(Notes notes) {
        return notesRepository.save(notes);
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

    public List<Notes> updateEntityFromOtherEntity(List<Notes> notesList, Long entityId, String entityType) throws Exception {
        String responseMsg;
        List<Notes> responseNotes = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entityId, entityType);
            Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
            Page<Notes> notes = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Notes> hashMap = notes.stream()
                    .collect(Collectors.toMap(Notes::getId, Function.identity()));
            List<Notes> notesRequestList = new ArrayList<>();
            if (notesList != null && notesList.size() != 0) {
                for (Notes request : notesList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    notesRequestList.add(request);
                }
                responseNotes = saveEntityFromOtherEntity(notesRequestList, entityId, entityType);
            }
            deleteNotes(hashMap);
            return responseNotes;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Notes> saveEntityFromOtherEntity(List<Notes> notesRequests, Long entityId, String entityType) {
        List<Notes> res = new ArrayList<>();
        for(Notes req : notesRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Notes> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Notes is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteNotes(Map<Long, Notes> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    public List<Notes> updateEntityFromOtherEntity(List<Notes> notesList, Long entityId, String entityType, List<Notes> oldEntityList) throws Exception {
        String responseMsg;
        List<Notes> responseNotes = new ArrayList<>();
        Map<UUID, Notes> notesMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Notes entity:
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
                    if(oldEntity != null) {
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
            notesMap.forEach((s, notes) ->  hashMap.put(notes.getId(), notes));
            deleteNotes(hashMap);
            return responseNotes;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
