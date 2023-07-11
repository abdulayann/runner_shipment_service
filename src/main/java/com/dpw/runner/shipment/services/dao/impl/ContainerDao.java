package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

@Repository
@Slf4j
public class ContainerDao implements IContainerDao {
    @Autowired
    private IContainerRepository containerRepository;

    @Override
    public Containers save(Containers containers) {
        return containerRepository.save(containers);
    }

    public List<Containers> saveAll(List<Containers> containersList) {
        return containerRepository.saveAll(containersList);
    }

    @Override
    public Page<Containers> findAll(Specification<Containers> spec, Pageable pageable) {
        return containerRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Containers> findById(Long id) {
        return containerRepository.findById(id);
    }

    @Override
    public void delete(Containers containers) {
        containerRepository.delete(containers);
    }

    public List<Containers> updateEntityFromShipment(CommonRequestModel commonRequestModel) throws Exception
    {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            List<ContainerRequest> containerList = new ArrayList<>();
            List<ContainerRequest> requestList = (List<ContainerRequest>) commonRequestModel.getDataList();
            if(requestList != null && requestList.size() != 0)
            {
                for(ContainerRequest request: requestList)
                {
                    containerList.add(request);
                }
                responseContainers = saveContainers(containerList);
            }
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Containers> saveContainers(List<ContainerRequest> containers)
    {
        List<Containers> res = new ArrayList<>();
        for(ContainerRequest req : containers){
            Containers saveEntity = convertToClass(req, Containers.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<Containers> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Container is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                saveEntity = oldEntity.get();
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }
}
