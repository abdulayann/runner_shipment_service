package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.CommonContainers;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ICommonContainersRepository extends JpaRepository<CommonContainers, Long> {
    List<CommonContainers> findAllByIdIn(List<Long> ids);
}