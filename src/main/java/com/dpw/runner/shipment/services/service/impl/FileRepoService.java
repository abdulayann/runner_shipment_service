package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EntityIdAndTypeRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.response.FileRepoResponse;
import com.dpw.runner.shipment.services.entity.FileRepo;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IFileRepoDao;
import com.dpw.runner.shipment.services.service.interfaces.IFileRepoService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class FileRepoService implements IFileRepoService {
    @Autowired
    private IFileRepoDao fileRepoDao;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        FileRepoRequest request = null;
        request = (FileRepoRequest) commonRequestModel.getData();
        FileRepo fileRepo  = mapToEntityFromRequest(request);
        fileRepo = fileRepoDao.save(fileRepo);
        return ResponseHelper.buildSuccessResponse(convertToResponse(fileRepo));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        FileRepoRequest request = (FileRepoRequest) commonRequestModel.getData();
        long id =request.getId();
        Optional<FileRepo> oldEntity = fileRepoDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("File Repo is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        FileRepo fileRepo = mapToEntityFromRequest(request);
        fileRepo.setId(oldEntity.get().getId());
        fileRepo = fileRepoDao.save(fileRepo);
        return ResponseHelper.buildSuccessResponse(convertToResponse(fileRepo));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<FileRepo>, Pageable> tuple = fetchData(request, FileRepo.class);
            Page<FileRepo> FileRepoPage  = fileRepoDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertListResponse(FileRepoPage.getContent()),
                    FileRepoPage.getTotalPages(),
                    FileRepoPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            Pair<Specification<FileRepo>, Pageable> tuple = fetchData(request, FileRepo.class);
            Page<FileRepo> FileRepoPage  = fileRepoDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertListResponse(FileRepoPage.getContent()),
                    FileRepoPage.getTotalPages(),
                    FileRepoPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<FileRepo> fileRepo = fileRepoDao.findById(id);
            if(!fileRepo.isPresent()) {
                log.debug("File Repo is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            fileRepoDao.delete(fileRepo.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<FileRepo> fileRepo = fileRepoDao.findById(id);
            if(!fileRepo.isPresent()) {
                log.debug("File Repo is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            FileRepoResponse response = convertToResponse(fileRepo.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveByEntityIdAndEntityType(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            EntityIdAndTypeRequest request = (EntityIdAndTypeRequest) commonRequestModel.getData();
            List<FileRepo> fileRepoList = fileRepoDao.findByEntityIdAndEntityType(request.getEntityId(), request.getEntityType());
            if(fileRepoList.size() == 0){
                ResponseHelper.buildListSuccessResponse(null);
            }
            return ResponseHelper.buildListSuccessResponse(convertListResponse(fileRepoList));

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertListResponse(List<FileRepo> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(fileRepo -> {
            responseList.add(convertToResponse(fileRepo));
        });
        return responseList;
    }

    private FileRepo mapToEntityFromRequest(FileRepoRequest request){
        return modelMapper.map(request, FileRepo.class);
    }
    private FileRepoResponse convertToResponse(FileRepo fileRepo){
        return modelMapper.map(fileRepo, FileRepoResponse.class);
    }

}
