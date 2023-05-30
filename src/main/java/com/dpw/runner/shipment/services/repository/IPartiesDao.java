package com.dpw.runner.shipment.services.repository;

import com.dpw.runner.shipment.services.entity.PartiesDetails;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IPartiesDao extends JpaRepository<PartiesDetails, Integer> {



}
