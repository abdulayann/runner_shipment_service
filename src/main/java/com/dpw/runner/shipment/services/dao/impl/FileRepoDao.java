package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IFileRepoDao;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.repository.interfaces.IFileRepoRepository;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@Repository
@Slf4j
public class FileRepoDao implements IFileRepoDao {
    @Autowired
    private IFileRepoRepository fileRepoRepository;

    @Override
    public FileRepo save(FileRepo fileRepo) {
        return fileRepoRepository.save(fileRepo);
    }

    @Override
    public Page<FileRepo> findAll(Specification<FileRepo> spec, Pageable pageable) {
        return fileRepoRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<FileRepo> findById(Long id) {
        return fileRepoRepository.findById(id);
    }

    @Override
    public void delete(FileRepo fileRepo) {
        fileRepoRepository.delete(fileRepo);
    }

    @Override
    public List<FileRepo> findByEntityIdAndEntityType(Long entityId, String entityType) {
        return fileRepoRepository.findByEntityIdAndEntityType(entityId, entityType);
    }

    public List<FileRepo> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<FileRepo> responseFileRepo = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(shipmentId, Constants.SHIPMENT_TYPE);
            Pair<Specification<FileRepo>, Pageable> pair = fetchData(listCommonRequest, FileRepo.class);
            Page<FileRepo> fileRepos = findAll(pair.getLeft(), pair.getRight());
            Map<Long, FileRepo> hashMap = fileRepos.stream()
                    .collect(Collectors.toMap(FileRepo::getId, Function.identity()));
            List<FileRepoRequest> fileReposRequestList = new ArrayList<>();
            List<FileRepoRequest> requestList = (List<FileRepoRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (FileRepoRequest request : requestList) {
                    Long id = request.getId();
                    request.setEntityId(shipmentId);
                    request.setEntityType(Constants.SHIPMENT_TYPE);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    fileReposRequestList.add(request);
                }
                responseFileRepo = saveFileRepo(fileReposRequestList);
            }
            deleteFileRepo(hashMap);
            return responseFileRepo;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private List<FileRepo> saveFileRepo(List<FileRepoRequest> fileRepos) {
        List<FileRepo> res = new ArrayList<>();
        for(FileRepoRequest req : fileRepos){
            FileRepo saveEntity = convertToClass(req, FileRepo.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<FileRepo> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }

    private void deleteFileRepo(Map<Long, FileRepo> hashMap) {
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
