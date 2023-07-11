package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
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

    public List<Notes> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<Notes> responseNotes = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
            Page<Notes> routings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Notes> hashMap = routings.stream()
                    .collect(Collectors.toMap(Notes::getId, Function.identity()));
            List<NotesRequest> notesRequestList = new ArrayList<>();
            List<NotesRequest> requestList = (List<NotesRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (NotesRequest request : requestList) {
                    Long id = request.getId();
                    request.setEntityId(shipmentId);
                    request.setEntityType(Constants.SHIPMENT_TYPE);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    notesRequestList.add(request);
                }
                responseNotes = saveNotes(notesRequestList);
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

    private List<Notes> saveNotes(List<NotesRequest> notesRequests) {
        List<Notes> res = new ArrayList<>();
        for(NotesRequest req : notesRequests){
            Notes saveEntity = convertToClass(req, Notes.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<Notes> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Notes is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
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
}
